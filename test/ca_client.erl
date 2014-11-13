-module(ca_client).
-author('yunnet@gmail.com').
-vsr('1.0').
-export([start/0]). 

-define(Debug2(F, D), (io:format(F, D))).
-define(Warn2(F, D), (io:format(F, D))).
-define(Error2(F, D), (io:format(F, D))).
-define(Info2(F), (io:format(F))).

start() ->
  spawn(fun() -> clientSock("192.168.4.212", 6799) end).  
  
clientSock(Host, Port) ->
  case gen_tcp:connect(Host, Port, [{active, true}]) of
	{ok, Socket} ->
			?Warn2("~p> host:~p port:~p connect ok!~n", [calendar:local_time(),Host, Port]),
			loop(Socket);				 
	_ -> 
		?Info2("connect fiald.")
  end.

  
loop(Socket) ->
  receive
	{tcp, Socket, Bin} ->
		Bin2 = lists:sublist([Bin], 10),		
		io:format("~p~n",  Bin2),
		loop(Socket);
	{tcp_closed, Socket} ->
		?Info2("socket is closed.");
	{tcp_error, Socket, Reason} ->		
		?Error2("socket err: ~p~n", [Reason])  
  end.