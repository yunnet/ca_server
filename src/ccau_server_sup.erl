%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2013 by  <>
%%%-------------------------------------------------------------------
-module(ccau_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    %%启用数据反应堆 
    case ca_data_reactor:start_link() of                    
	{ok, RePid} -> 
	    io:format("reactor start pid:~p.~n", [RePid]),
	    {ok, RePid};
	Other1 ->
	    io:format("reactor err:~p.~n", [Other1]),
	    {error, Other1}
    end,

    %%启动Client管理者
    case ccau_client_mgr:start_link() of                    
	{ok, RePid2} -> 
	    io:format("client manager start pid:~p.~n", [RePid2]),
	    {ok, RePid2};
	Other2 ->
	    io:format("client manager err:~p.~n", [Other2]),
	    {error, Other2}
    end,   

    {ok, Socket} = gen_tcp:listen(Port, [{active, true}]),
    R = supervisor:start_link({local, ?SERVER}, ?MODULE, [Socket]),
    case R of
	{ok, SupPid} ->
	    io:format("supervisor pid:~p.~n", [SupPid]),
            start_child(),	   
            {ok, SupPid};
        Other ->
            {error, Other}
    end.   

start_child() ->
    supervisor:start_child(?SERVER, []).

init([Socket]) ->
    Server = {ccau_server, {ccau_server, start_link, [Socket]},
              temporary, brutal_kill, worker, [ccau_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.


