%%%-------------------------------------------------------------------
%% @doc oving_3_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(oving_3_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  UDP_responders = [ udp_module:udp_resp_spec(Station_no) || Station_no <- lists:seq(0,10)],
    UDP_children = UDP_responders ++ [
                #{id => udp_broadcaster, start => {udp_module, start_udp_broadcaster, []}}
                %#{id => udp_listener, start => {udp_module, start_udp_listener, []}}
                ],

    TCP_children = [
      #{id => tcp_terminated, start => {tcp_module, start_tcp_terminated, []}}
      % #{id => tcp_sender, start => {tcp_module, start_tcp_sender, []}}
      #{id => start_tcp_length, start => {tcp_module, start_tcp_length, []}}

    ], 
    Children = TCP_children  ++ UDP_children,
    {ok, {{one_for_one, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
