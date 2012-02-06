-module(prc_extension).

-compile(export_all).

fallback(_X) ->
	_Y = {value, foo},
	_Z = {result, bar},
	false.
