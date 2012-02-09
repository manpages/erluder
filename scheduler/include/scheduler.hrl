-record(nodeT,{
	address,
	type,
	state={idle,[{task, undefined}, {cmd, undefined}, {result, undefined}]},
	since
}).

-record(taskT,{
	id,
	seed,
	data
}).
