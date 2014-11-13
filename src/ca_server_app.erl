%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by  <yunnet>
%%%-------------------------------------------------------------------
-module(ca_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).    

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    case ca_server_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    {error, Other}
    end.


stop(_State) ->
    ok.

	
	
