-module(ca_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
 
-define(CCAD_PORT, 6789).    %%listen terminal send data.  侦听客户端发来的数据
-define(CCAU_PORT, 6799).    %%listen client send data.

-define(DEF_SERVICE, ccad_echo_fsm). 
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
     CCAD_mgr ={ccad_server_sup, {ccad_server_sup, start_link, [?CCAD_PORT, ?DEF_SERVICE]},
	       permanent,                            
	       2000,                                 
	       supervisor,                            
	       [ccad_server_sup]                     
	      },
     CCAU_mgr ={ccau_server_sup, {ccau_server_sup, start_link, [?CCAU_PORT]},
	       permanent,                            
	       2000,                                 
	       supervisor,                            
	       [ccau_server_sup]                     
	      },

    Children = [CCAD_mgr, CCAU_mgr],
    RestartStrategy = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
    {ok, {RestartStrategy, Children}  }.


