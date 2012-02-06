-module(sch_remote).

-include("scheduler/include/scheduler.hrl").

-compile(export_all).

hello(Node, Type) ->
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
	{ok, fission_syn:get({node, Node})}
.

started(Node, Task, Cmd) ->
	fission_tuple:set(
		{node, Node}, #nodeT.state, {working, [Task, Cmd, {result, undefined}]}
	),
	lists:foreach(fun (X) -> 
			rpc:async_call(X, sch_remote, started, [
				Node, erlang:nth(2, Task), erlang:nth(2, Cmd)
			])
		end,
		fission_syn:get_def({lists, application}, [])
	),
	{ok, fission_syn:get({node, Node})}
.

done(Node, Task, Cmd, Result) ->
	fission_tuple:set(
		{node, Node}, #nodeT.state, {idle, [Task, Cmd, Result]}
	),
	lists:foreach(fun (X) -> 
			rpc:async_call(X, sch_remote, done, [
				Node, erlang:nth(2, Task), erlang:nth(2, Cmd), erlang:nth(2, Result)
			])
		end,
		fission_syn:get_def({lists, application}, [])
	),
	{ok, fission_syn:get({node, Node})}
.	
