-module(prc_loop).

-compile(export_all).

start() ->
	io:format("ПЫЩ ПЫЩ ОЛОЛО~n"),
	case whereis(prc_main) of 
		undefined -> ok;
		_ -> unregister(prc_main)
	end,
	PID = spawn(?MODULE, loop, []),
	erlang:register(prc_main, PID),
	{ok, PID}
.

get_overseer() ->
	fission_syn:get_def(overseer, false)
.

set_overseer(Node) -> 
	case is_atom(Node) of
		true  -> fission_syn:set(overseer, Node);
		false -> fission_syn:set(overseer, list_to_atom(Node))
	end
.

exec(X) -> 
	case fission_syn:get({cmd, X}) of 
		{value, Bin} -> os:cmd(Bin ++ " " ++ params(X));
		false -> false
	end
.

params(X) ->
	case fission_syn:get({params, X}) of
		{value, Params} -> Params;
		false -> ""
	end
.

loop() ->
	io:format("~p:loop>> Entered~n", [?MODULE]),
	receive 
		stop -> 
			io:format("~p:loop>> Received 'stop'. Unregistering and exiting loop~n", [?MODULE]),
			case whereis(prc_main) of 
				undefined -> ok;
				_ -> unregister(prc_main)
			end,
			ok;
		start ->	
			OverNode = get_overseer(),
			case net_adm:ping(OverNode) of 
				pang -> 
					erlang:error("prc_loop:loop>> Can't reach overseer, or overseer isn't defined.");
				pong -> 
					io:format("~p:loop>> ~p reached, sending keepalive and waiting for orders~n", [?MODULE, OverNode]),
					RetVal = rpc:call(OverNode, sch_remote, keepalive, [node()]),
					io:format("~p~n", [RetVal]);
				_    ->
					erlang:error("prc_loop:loop>> Not gonna happen.")
			end,
			loop();
		X ->
			io:format("~p:~p>> ~p not handled", [?MODULE, "loop", X])
	end
.
