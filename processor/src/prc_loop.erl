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

run_cmd(OScmd) ->
	prc_main ! {started, {self(), OScmd}},
	prc_main ! {done, {self(), os:cmd(OScmd)}},
	ok
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
	OScmd = case fission_syn:get({cmd, X}) of 
		{value, Bin} -> 
			Bin ++ " " ++ params(X);
		false -> 
			case prc_extension:get_cmd(X) of
				{value, Xbin} -> 
					Xbin ++ " " ++ params(X);
				false ->
					false
			end
	end,
	case OScmd of
		false -> not_found;
		_ -> {ok, spawn(?MODULE, run_cmd, [OScmd])}
	end
.
exec(Bin,Params) -> {ok, spawn(?MODULE,  run_cmd, [Bin ++ " " ++ Params])}.

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
					?debugFmt("Remote call returned: ~p~n", [RetVal]);
				_    ->
					?debugMsg("w.t.f"),
					erlang:error("Not gonna happen.")
			end,
			loop();
		{run
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
