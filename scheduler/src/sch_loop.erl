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

get_top_task(raw) -> 
	case  fission_zset:part_left(queue,0,1) of 
		{result, [{RawTask, TaskID}]} -> RawTask;
		_ -> false
	end
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
			case  fission_zset:part_left(queue,0,1) of 
				{result, [{RawTask, TaskID}]} -> 
					?debugFmt("Task ~p found.~nRaw task:~p~n representation: ~p", [
						TaskID, 
						RawTask, 
						sch_extension:decode_task(RawTask)
					]),
					case rpc:call(Node, prc_loop, exec, [sch_extension:decode_task(TaskID)]) of
						{ok, _PID} -> 
							?debugFmt("The task is successfully assigned to ~p", [Node]);
						not_found ->
							?debugFmt("~p can't perform the task", [Node]),
							fission_list:push({task, RawTask, blacklist}, Node);
						{result, Res} ->
							?debugFmt("The task is solved by ~p's processor extension", [Node]),
							sch_remote:done(Node, RawTask, undefined, Res)
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
					fission_syn:get_def({lists, processor}, []) -- fission_syn:get_def({task, get_top_task(raw), blacklist}, [])
				)
			),
			loop();
		X ->
			?debugFmt("Message received: ~n~p", [X]),
			loop()
	end
.
