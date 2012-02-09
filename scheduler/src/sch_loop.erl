-module(sch_loop).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

start() ->
	?debugMsg("Starting fission"),
	case application:start(fission) of
		ok -> ok;
		{error, {already_started, fission}} -> ok;
		_  -> erlang:error("Fission failed to start")
	end,
	case whereis(sch_main) of 
		undefined -> ok;
		_ -> unregister(sch_main)
	end,
	PID = spawn(?MODULE, loop, []),
	erlang:register(sch_main, PID),
	?debugMsg("Starting processor loop, registered as sch_main"),
	{ok, PID}
.

get_node(Nodes) -> % TODO: Y-combinator, anyone?
	lists:foldl(fun(Node, []) ->
			case net_adm:ping(Node) of
				pong -> Node;
				_ -> []
			end
		;(_, _) -> [] end,
		[],
		Nodes
	)
.

loop() ->
	?debugMsg("Entered main loop"),
	net_kernel:monitor_nodes(false), %dirty-dirty
	net_kernel:monitor_nodes(true),
	receive
		stop -> 
			?debugMsg("Received 'stop'. Unregistering and exiting loop"),
			case whereis(sch_main) of 
				undefined -> ok;
				_ -> unregister(sch_main)
			end,
			ok;
		{assign, Node} ->
			?debugFmt("~p is idle, searching for task to assign", [Node]),
			case fission_zset:part_left(
				queue,
				sch_task:get_caret() - sch_task:get_top_seed(),
				1
			) of 
				{result, [{Seed, TaskID}]} -> 
					?debugFmt("Found. Task:~n~p~nSeed:~p~n", [
						sch_task:get(TaskID), 
						Seed
					]),
					case rpc:call(Node, prc_loop, exec, [sch_task:get(TaskID)]) of
						{ok, _PID} -> 
							sch_task:pop_caret(),
							fission_list:push(assigned, TaskID),
							?debugFmt("The task is successfully assigned to ~p", [Node]);
						not_found ->
							?debugFmt("~p can't perform the task", [Node]),
							fission_list:push({task, TaskID, blacklist}, Node);
						{result, Res} ->
							sch_task:pop_caret(),
							fission_list:push(assigned, TaskID),
							?debugFmt("The task is solved by ~p's processor extension", [Node]),
							sch_remote:done(Node, {task, Seed}, {cmd, undefined}, {result, Res}) % <- nice arg format huh?
					end;
				_ -> 
					?debugMsg("No tasks in queue"),
					false
			end,
			loop();
		queue ->
			?debugMsg("New task is queued. Searching for node to assign task to"),
			sch_remote:roll_node(
				get_node(
					fission_syn:get_def(
						{lists, processor}, 
						[]
					) -- fission_syn:get_def(
						{task, erlang:element(2, sch_task:get_top()), blacklist}, 
						[]
					)
				)
			),
			loop();
		X ->
			?debugFmt("Message received: ~n~p", [X]),
			loop()
	end
.
