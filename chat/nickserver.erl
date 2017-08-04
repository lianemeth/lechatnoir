-module(nickserver).

-export([start/0]).

start() ->
	register('NickServer', spawn(fun() -> loop() end)).

loop()->
	receive
		{From, Msg}->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.

