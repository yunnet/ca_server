%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2014 by  <>
%%%-------------------------------------------------------------------
-module(ca_data_reactor).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([sendTerminal/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LOOP, 255).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, count=0, lease_time, start_time}).

%%%===================================================================
%%% API
%%%===================================================================
sendTerminal(Value) ->
    gen_server:cast(?SERVER,  {recv, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),   
   
    {ok, #state{value=[],
		count=0,
                lease_time = ?DEFAULT_LEASE_TIME,
		start_time = StartTime
	       }, 
     time_left(StartTime, ?DEFAULT_LEASE_TIME)
    }.


time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime =  calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({recv, Value}, State) ->    
    #state{value=V1, count=C1, lease_time=L1, start_time=S1} = State,

    case C1 >= ?DEFAULT_LOOP of
	true -> 
	    ccau_server:transfer_Data([Value|V1]),
	    State2 = #state{value = [],
			    count = 0,
			    lease_time = L1,
			    start_time = S1
			    };	
	false ->
	    State2 = #state{value = [Value|V1],
			    count = C1 + 1,
			    lease_time = L1,
			    start_time = S1
			    } 
    end,
    TimeLeft = time_left(S1, L1),
    {noreply, State2, TimeLeft}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
