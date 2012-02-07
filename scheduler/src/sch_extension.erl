-module(sch_extension).

-compile(export_all).

decode_task(X) -> X.

encode_task(X) -> X.

handle_result(T,R) -> fission_syn:set({task, T, result}, R).
