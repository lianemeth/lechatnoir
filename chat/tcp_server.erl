-module(tcp_server).
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/4]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
		port,
		loop,
		args,
		ip=any,
		lsocket=null}).

start(Name, Port, Loop, Args) ->
	State = #server_state{port = Port, loop = Loop, args=Args},
	% register locally a server as Name
	% Callback functions are found in ?MODULE (init, handle_call, handle_cast)
	% State is passed to init
	% [] is a empty list of options
	gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, LSocket} ->
   			NewState = State#server_state{lsocket = LSocket},
   			{ok, accept(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end.

handle_cast({accepted, _Pid}, State=#server_state{})->
	{noreply, accept(State)}.

handle_call({accepted, _Pid}, Caller, State=#server_state{})->
	{stop, {unknown_call, Caller}, State}.

accept_loop({Server, LSocket, {Module, Function}, Args}) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	gen_server:cast(Server, {accepted, self()}),
	Module:Function(Socket, Args).

accept(State=#server_state{lsocket=LSocket, loop=Loop, args=Args}) ->
	spawn(?MODULE, accept_loop, [{self(), LSocket, Loop, Args}]),
	State.

handle_info(_Msg, Library) -> {noreply, Library}.

terminate(Reason, _Library) -> {ok, Reason}.

code_change(_Old, Library, _Extra) -> {ok, Library}.

