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
					state=idle,
					since=erlang:now()
				}
			),
			fission_list:push(Type, Node),
			ok;
		_ -> 
			false
	end,
	{ok, fission_syn:get({node, Node})}
.

started(Node, Task, Cmd) ->
	fission_tuple:set(
		{node, Node},#nodeT.state, 
		{working, [
			{task, Task}, 
			{cmd, Cmd}
		]}
	),

