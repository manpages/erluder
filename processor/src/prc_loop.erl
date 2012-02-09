-module(prc_loop).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

start() ->
	?debugMsg("Starting fission"),
	case application:start(fission) of
		ok -> ok;
		_  -> erlang:error("Fission failed to start")
	end,
	case whereis(prc_main) of 
		undefined -> ok;
		_ -> unregister(prc_main)
	end,
	PID = spawn(?MODULE, loop, []),
	erlang:register(prc_main, PID),
	?debugMsg("Starting processor loop, registered as prc_main"),
	prc_main ! start,
	{ok, PID}
.

run_cmd(OScmd, Task) ->
	?debugFmt("~p", [OScmd]),
	prc_main ! 
		{started, [
			{pid, self()},
			{cmd, OScmd},
			{task, Task}
		]},
	prc_main ! 
		{done, [
			{pid, self()}, 
			{cmd, OScmd},
			{result, os:cmd(OScmd)},
			{task, Task}
		]},
	{ok, self()}
.

get_overseer() ->
	fission_syn:get_def(overseer, false)
.

set_overseer(Node) -> %TODO: handle dynamic change of overseer 
	case is_atom(Node) of
		true  -> fission_syn:set(overseer, Node);
		false -> fission_syn:set(overseer, list_to_atom(Node))
	end
.

exec(X) -> 
	?debugFmt("***~nStuff to do!~nRaw: ~p~n***", [X]),
	OScmd = case fission_syn:get({cmd, X}) of 
		{value, Bin} -> 
			Bin ++ " " ++ params(X);
		false -> 
			case prc_extension:fallback(X) of
				{value, Xbin} -> 
					Xbin ++ " " ++ params(X);

				% we could just wildcard X -> X here, but we 
				% are referring to the 'protocol' that way

				{result, Result} ->   % !!!FOR FAST ERLANG STUFF
					{result, Result}; % ONLY!!!
				false ->
					false
			end
	end,
	case OScmd of
		{result, X} -> {result, X};
		false -> not_found;
		_ -> {ok, spawn(?MODULE, run_cmd, [OScmd, X])}
	end
.
exec(TaskID,Bin,Params) -> 
	{ok, spawn(?MODULE,  run_cmd, [Bin ++ " " ++ Params, TaskID])}.

params(X) ->
	case fission_syn:get({params, X}) of
		{value, Params} -> Params;
		false -> ""
	end
.

loop() ->
	?debugMsg("Entered main loop"),
	net_kernel:monitor_nodes(false), %dirty-dirty
	net_kernel:monitor_nodes(true),
	receive 
		stop -> 
			?debugMsg("Received 'stop'. Unregistering and exiting loop"),
			case whereis(prc_main) of 
				undefined -> ok;
				_ -> unregister(prc_main)
			end,
			ok;
		start ->	
			OverNode = get_overseer(),
			case net_adm:ping(OverNode) of 
				pang -> 
					?debugFmt("Can't reach overseer '~p', or overseer isn't defined.", [OverNode]);
				pong -> 
					?debugFmt("~p reached, saying hi and waiting for orders", [OverNode]),
					RetVal = rpc:call(OverNode, sch_remote, hello, [node(), processor]),
					?debugFmt("Remote call returned: ~p~n", [RetVal])
			end,
			loop();
		{started, Info} ->
			?debugFmt("Processor is running ~p", [lists:keyfind(cmd, 1, Info)]),
			rpc:async_call(get_overseer(), sch_remote, started, [
				node(), 
				lists:keyfind(task, 1, Info), 
				lists:keyfind(cmd, 1, Info)
			]),
			loop();
		{done, Info} ->
			?debugFmt("Processor finished running ~p~nResult: ~p", [
				lists:keyfind(cmd, 1, Info),
				lists:keyfind(result, 1, Info)
			]),
			rpc:async_call(get_overseer(), sch_remote, done, [
				node(), 
				lists:keyfind(task, 1, Info), 
				lists:keyfind(cmd, 1, Info),
				prc_extension:postprocess(lists:keyfind(result, 1, Info))
			]),
			loop();
		%utility stuff
		{nodedown, X} ->
			case (X == get_overseer()) of 
				true -> 
					?debugMsg("The overseer is down"),
					ok;
				_ ->
					ok
			end,
			loop();
		X ->
			?debugFmt("New message: ~p", [X]),
			loop()
	end
.
