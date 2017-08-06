-module(chat_noir_server).

-export([start/0, start/2, loop/2, accept_loop/1, server_test/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).


-record(client, {
		name,
		info,
		socket
	}).

-record(public, {
	  message,
	  socket
	 }).

-record(server_state, {
	  port,
	  nickserver,
	  socket=null}).

start()->
	start('LeChatNoir', 8888).

start(Name, Port)->
	NickServer = nickserver:start(),
	ServerState = #server_state{port = Port, nickserver = NickServer},
	register(Name, spawn(fun() -> init(ServerState) end)).


accept_loop(State=#server_state{socket=ListenSocket, nickserver=NickServer}) ->
	case  gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			spawn(?MODULE, loop, [AcceptSocket, NickServer]),
			accept_loop(State);
		{error, Reason} ->
			io:format(<<"WHAT">>),
			io:format(Reason),
			{stop, Reason}
	end.

init(ServerState = #server_state{port=Port, nickserver=NickServer}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, ListenSocket} ->
			NewState = ServerState#server_state{socket = ListenSocket, nickserver=NickServer},
			accept_loop(NewState);
		{error, Reason} ->
			{stop, Reason}
	end.


get_message_type([]) -> null;
get_message_type([<<"PUB">> | _]) -> public;
get_message_type([<<"NEW">> | _]) -> newclient;
get_message_type([<<"PVT">>|_]) -> private;
get_message_type([_]) ->  null.


parse_client(MsgList, Socket) ->
	Name = lists:nth(2, MsgList),
	Info = lists:nth(3, MsgList),
	#client{name=Name, 
	        info=Info, 
	        socket=Socket}.

parse_public(MsgList, Socket)->
	Message = lists:nth(2, MsgList),
	#public{message=Message,
	       socket=Socket}.


parse_message([]) ->
	[];
parse_message(<<"">>) ->
	[];
parse_message(SomeBinary) ->
	binary:split(SomeBinary, <<";">>,  [global, trim_all]). 


loop(Socket, NickServer) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			process_mesage(Data, Socket, NickServer);
		{error, closed} -> 
			{error, closed}
	end.



process_mesage(Data, Socket, NickServer) ->
	MessageList = parse_message(Data),
	case get_message_type(MessageList) of
		newclient ->
			NewClient = parse_client(MessageList, Socket),
			NickServer ! {self(), {newclient, NewClient}},
			loop(Socket, NickServer);
		public ->
			NewPublicMessage = parse_public(MessageList, Socket),
			NickServer ! {self(), {public, NewPublicMessage}},
			loop(Socket, NickServer);
		private ->
			loop(Socket, NickServer);
		_ ->
			gen_tcp:send(Socket, "Error\n"),
			gen_tcp:close(Socket)
	end.


server_test() ->
	[<<"NEW">>, <<"DOUGLAS">>, <<"ADADA">>] = parse_message(<<"NEW;DOUGLAS;ADADA">>),
	NewClientList = parse_message(<<"NEW;DOUGLAS;ADADA">>),
	newclient = get_message_type(NewClientList),
	public = get_message_type([<<"PUB">>, <<"SOMETHING">>]),
	private = get_message_type([<<"PVT">>,<<"SOMEONE">>, <<"SOMETHING">>]),
	{client, <<"DOUGLAS">>, <<"ADADA">>, 123} = parse_client(NewClientList, 123),
	NewPublicMsg= parse_message(<<"PUB;;SOMETHING">>),
	{public, <<"SOMETHING">>, 123} = parse_public(NewPublicMsg, 123).
