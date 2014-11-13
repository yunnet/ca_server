%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Oct 2013 by  <>
%%%-------------------------------------------------------------------
-module(ccau_client_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([addClient/1, delClient/1, findClient/1, getNextClient/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TABLE_1, ?MODULE).
-include("ca_common.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

addClient(ClientInfo) ->
    gen_server:call(?MODULE, {add, ClientInfo}).

delClient(Key)->
    gen_server:call(?MODULE, {delete, Key}).

findClient(Key)->
    gen_server:call(?MODULE, {find, Key}).



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
    ets:new(clientinfo, [public,
		         ordered_set,
		         named_table,
			{keypos, #clientinfo.id}
		        ]),
    {ok, #state{}}.

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
handle_call({add, ClientInfo}, _From, State) ->
    ets:insert(clientinfo, ClientInfo),
    Reply = ok,
    {reply, Reply, State};
handle_call({delete, Key}, _From, State) ->
    ets:delete(clientinfo, Key),
    Reply = ok,
    {reply, Reply, State};
handle_call({find, Key}, _From, State) ->
    Reply = getNextClient(Key),
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
handle_cast(_Msg, State) ->
    {noreply, State}.

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

getNextClient([])->    
    case ets:first(clientinfo) of
	'$end_of_table'->
	    [];
	Key->
	    ets:lookup(clientinfo, Key)
    end;
getNextClient(Key)->	
    case ets:next(clientinfo, Key) of
	'$end_of_table'->
	    [];
	Next->
	    ets:lookup(clientinfo, Next)
    end.
