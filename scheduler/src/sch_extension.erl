-module(sch_extension).

-include_lib("eunit/include/eunit.hrl").
-include("scheduler/include/scheduler.hrl").

-compile(export_all).

decode_task(X) -> X.

encode_task(X) -> X.

handle_result(T,R) -> 
	?debugFmt("Task ~p resulted in ~p~n", [T,R]),
	fission_syn:set({task, T, result}, R).
