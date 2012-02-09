-module(sch_task).

-include_lib("eunit/include/eunit.hrl").
-include("scheduler/include/scheduler.hrl").

-export([
	get/1,
	get_top/0,
	get_top_task/0,
	queue_task/1,
	queue_task/2,
	get_top_seed/0,
	get_caret/0,
	pop_caret/0,
	swap_tasks/2
]).

get_top() -> 
	case fission_zset:part_left(queue,0,1) of 
		{result, [{Seed, TaskID}]} -> {Seed, TaskID};
		_ -> false
	end
.
get_top_task() -> 
	case get_top() of
		{_,ID} -> sch_task:get(ID);
		false -> false
	end
.


get_top_seed() -> 
	case get_top() of
		{Seed,_} -> Seed;
		false -> 0
	end
.

get_caret() ->
	fission_syn:get_def(caret,  0).

push_caret() ->
	fission_syn:inc_v(caret, -1).

pop_caret() ->
	fission_syn:inc_v(caret, 1).


get_last_seed() -> 
	case get_top_seed() of
		false -> 0;
		TopSeed -> fission_zset:size_v(queue) - 1 + TopSeed
	end
.

get(TaskID) ->
	fission_syn:get_v({task, TaskID}).

queue_task(DecodedTask) -> 
	queue_task(DecodedTask, bottom)
.

queue_task(DecodedTask, TopOrBottom) -> 
	Data = sch_extension:encode_task(DecodedTask),
	TID  = fission_syn:inc_v(task),
	case TopOrBottom of
		top ->
			NewSeed = push_caret(),
			fission_zset:set(
				queue, 
				TID,
				NewSeed
			),
			fission_syn:set({task, TID},
				#taskT{
					id   = TID,
					seed = NewSeed,
					data = Data
				}
			);
		bottom ->
			NewSeed = get_last_seed() + 1,
			fission_zset:set(
				queue,
				TID,
				NewSeed
			),
			fission_syn:set({task, TID},
				#taskT{
					id   = TID,
					seed = NewSeed,
					data = Data
				}
			)
	end,
	fission_syn:set({task, TID, stats}, [{queued, erlang:now()}])
.

swap_tasks(T1, T2) ->
	T1S = fission_tuple:get_v({task, T2}, #taskT.seed),
	T2S = fission_tuple:get_v({task, T1}, #taskT.seed),
	fission_tuple:set({task, T1}, #taskT.seed, T1S),
	fission_tuple:set({task, T2}, #taskT.seed, T2S),
	fission_zset:set(queue, T1, T1S),
	fission_zset:set(queue, T2, T2S)
.
