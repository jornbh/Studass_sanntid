-module(tcp_module).
-export([
  start_tcp_terminated/0
  , start_tcp_sender/0
  , start_tcp_length/0
]).

%% Terminated Socket
start_tcp_terminated()->
    {ok, spawn( fun tcp_terminated/0)}.


tcp_terminated()->
  {ok, ListenSocket} = gen_tcp:listen(8091, [binary, {active,false}]),
  tcp_listen_loop(ListenSocket, []).

tcp_listen_loop(ListenSocket, [] )->
  io:format("Started ~p ~n", [listener]),
  {ok, Accept_socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> tcp_terminated_connection(Accept_socket, []) end),
  tcp_listen_loop(ListenSocket,[]).

tcp_terminated_connection(Socket, Current_message)->
  io:format("new tcp_terminated_connection ~n"),
  inet:setopts(Socket, [list, {active, false}]),
  case gen_tcp:recv(Socket, 0) of 
    {ok, Message} -> 
      Not_zero = fun(X)-> X =/= 0 end, 
      case  lists:splitwith( Not_zero, Current_message++ Message) of 
        {Remainder, [] }-> ok;
        {Reply, [0 |Remainder]} -> 
          gen_tcp:send( Socket,"You said: " ++ Reply ++[0] ),
          % io:format("Split ~p ~n", [{Reply, Remainder}]),
          ok
      end,
      io:format("Term got~p ~n", [Message])  
  end,
  tcp_terminated_connection(Socket, Remainder).






%% 1024 socket


start_tcp_length()->
    {ok, spawn( fun tcp_length/0)}.


tcp_length()->
  {ok, ListenSocket} = gen_tcp:listen(8092, [binary, {active,false}]),
  tcp_listen_length_loop(ListenSocket, []).

tcp_listen_length_loop(ListenSocket, [] )->
  io:format("Started ~p ~n", [listener]),
  {ok, Accept_socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> tcp_length_connection(Accept_socket, []) end),
  tcp_listen_length_loop(ListenSocket,[]).

tcp_length_connection(Socket, _Current_message)->
  io:format("new tcp_length_connection ~n"),
  inet:setopts(Socket, [list, {active, false}]),
  case gen_tcp:recv(Socket, 1024) of 
    {ok, Reply} -> 

      gen_tcp:send( Socket,"You said: " ++ Reply ++[0] ),

      % io:format("Term got~p ~n", [Reply])  ,
      ok 
  end,
  tcp_length_connection(Socket, []).




%% Debug sender
start_tcp_sender()-> 
  {ok, spawn_link( fun()-> tcp_sender() end )}. 

tcp_sender()-> 
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8092, [binary, {active,false}]), 
  tcp_sender_loop(Socket). 

tcp_sender_loop(Socket)-> 
  inet:setopts(Socket, [list, {active, false}]),
  Message = "A message" ++[0], 
  gen_tcp:send(Socket, Message),
  io:format("Sendt  ~p ~n", [Message]), 
  case gen_tcp:recv(Socket, 0, 0) of
    {error, _Reason} -> ok; %io:format("{Error  ~p}", [Reason]);
    {ok,Msg} -> io:format("Got in return:  ~p ~n", [Msg])
  end, 

  timer:sleep(50),
  tcp_sender_loop(Socket). 


