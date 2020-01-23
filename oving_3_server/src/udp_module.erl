-module(udp_module).

-export( [  start_udp_broadcaster/0
          , start_udp_listener/0
          , start_udp_resp/1
          , udp_resp_spec/1
          ]).



start_udp_broadcaster( )->
  {ok, Socket} = gen_udp:open(8789, [binary, {active,false}, {broadcast, true}]),
  {ok, spawn( fun()->bcast_loop(Socket)end)}.


bcast_loop( Socket)->
  IP = {255,255,255,255},
  %IP = {127,0,0,1},
  Message = get_IP_str(),
  gen_udp:send(Socket, IP , 30000, Message),
  case gen_udp:recv(Socket, 0,0) of
    {ok, Msg} -> io:format("Got unexpected messagee~p~n", [Msg]);
    _Other -> ok % io:format("No message ~p ~n", [Other])
  end,
  timer:sleep(500),
  io:format("Bcst,"),
  bcast_loop(Socket).


start_udp_listener()->
  {ok, Socket} = gen_udp:open(30000, [binary, {active,false}]),

  {ok, spawn( fun()->listen_loop(Socket) end )}.
listen_loop(Socket)->
  io:format("R"),
  {ok, {Address, Port, Message}} = gen_udp:recv(Socket, 0),
  io:format( "GOt: ~p~n", [Message]),
  listen_loop(Socket).


get_IP_str()->
  {ok, Ifs} = inet:getif(),
  IPs = lists:map(fun( {IP, _broadaddr, _mask}) -> IP end, Ifs),
  Valid_ips = IPs -- [{127,0,0,1}],
  IP_tuple = hd(Valid_ips),
  (inet_parse:ntoa(IP_tuple)).


%% UDP-responder

udp_resp_spec(Server_number)->
#{    id => list_to_atom( "udp_responder_" ++ integer_to_list(Server_number) ),
      start => {?MODULE, start_udp_resp,[Server_number] }
  }.

start_udp_resp(Server_number)->
  {ok, Socket} = gen_udp:open(20000 + Server_number, [binary, {active,false}]),
  {ok, spawn( fun()-> udp_resp_loop(Socket) end)}.

udp_resp_loop( Socket)->

  {ok, {IP, Port, Message}} = gen_udp:recv(Socket, 0),
  io:format("Got message resp ~p~n", [Socket]),
  gen_udp:send( Socket, IP, Port, "You said: "++ Message),
  udp_resp_loop( Socket).
