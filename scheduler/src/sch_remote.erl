-module(sch_remote).

-include("scheduler/include/scheduler.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

hello(Node, Type) ->
	?debugFmt("args: Node: ~p; Type: ~p", [Node, Type]),
	case ((Type == processor) or (Type == application)) of %TODO: sch_config
		true -> 
			fission_syn:set(
				{node, Node}, 
				#nodeT{
					address=Node,
					type=Type,
					since=erlang:now()
				}
			),
			fission_list:push({lists, Type}, Node),
			ok;
		_ -> 
			false
	end,
	{ok, roll_node(Node)}
.

started(Node, Task, Cmd) ->
	fission_tuple:set(
		{node, Node}, #nodeT.state, {working, [Task, Cmd, {result, undefined}]}
	),
	lists:foreach(fun (X) -> 
			case net_adm:ping(X) of 
				ping -> rpc:async_call(X, sch_remote, started, [
					Node, erlang:element(2, Task), erlang:element(2, Cmd)
				]);
				_ -> false
			end
		end,
		fission_syn:get_def({lists, application}, [])
	),
	{ok, roll_node(Node)}
.


done(Node, Task, Cmd, Result) ->
	fission_tuple:set(
		{node, Node}, #nodeT.state, {idle, [Task, Cmd, Result]}
	),
	lists:foreach(fun (X) -> 
			case net_adm:ping(X) of 
				pong ->	rpc:async_call(X, sch_remote, done, [
					Node, erlang:element(2, Task), erlang:element(2, Cmd), erlang:element(2, Result)
				]);
				_ -> false
			end
		end,
		fission_syn:get_def({lists, application}, [])
	),
	sch_extension:handle_result(erlang:element(2, Task), erlang:element(2, Result)),
	{ok, roll_node(Node)}
.	

queue(Task, Score) ->
	fission_zset:set(queue, Score, sch_extension:encode_task(Task)),
	sch_main ! queue.


roll_node(Node) ->
	?debugFmt("Getting node ~p information", [Node]),
	case fission_syn:get({node, Node}) of
		{value, Val} ->
			?debugFmt("Node state is ~p. ~nProceeding", [Val]),
			case erlang:element(1, Val#nodeT.state) of
				idle -> sch_main ! {assign, Node};
				_ -> ok
			end,
			Val;
		_ -> false
	end
.
