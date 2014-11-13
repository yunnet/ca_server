%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2013 by  <>
%%%-------------------------------------------------------------------
-module(ccau_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, 
         handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
         terminate/2, 
	 code_change/3
	]).

-export([transfer_Data/1]).


-include("ca_common.hrl").

-record(state, {lsock,     %%listen
                pid,       %%Pid	
		key,       %%key  
	        socket     %%socket
                }).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
%%    io:format("Sock: ~p, PID: ~p~n", [LSock, self()]),
    {ok, #state{lsock = LSock, pid=self()}, 0}.


%%-------------------------------------------------------------------
handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast({data, Data},  #state{socket = Socket} = State) ->
%%    io:format("ccau recv: ~p~n", [Data]),
    gen_tcp:send(Socket, Data),
    {noreply, State};

handle_cast(stop, State) ->   
    {stop, normal, State}.

%%-------------------------------------------------------------------
%% 收到CCAU客户端数据
handle_info({tcp, Socket, RawData}, State) ->
    NewState = handle_data(Socket, RawData, State),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, #state{key=Key} = State) ->   
    ccau_client_mgr:delClient(Key),
    {stop, normal, State};

handle_info(timeout, #state{lsock = LSock, pid = Channel} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),

    {ok, {_, _}} = inet:peername(Socket),
    Key = getChannal_keys(Socket),                       %%key = tcp_192_168_2_211_4944
    io:format("client connect <~p> ok. ~n", [Key]),
    register(list_to_atom(Key), Channel),

    NewRec = #clientinfo{id = Key, 
                         socket = Socket, 
                         pid = Channel
                         },

    ccau_client_mgr:addClient(NewRec),      %% ets: add client 

    ccau_server_sup:start_child(),   
 
    NewState = #state{key = Key, socket=Socket},
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------
handle_data(Socket, RawData, State) ->
    <<6, 2>> = RawData,
    io:format("recv: ~p \n", [RawData]),
    try       
        gen_tcp:send(Socket, io_lib:fwrite("Reply:~p.~n", [RawData]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p.~n", [Err]))
    end,
    State.

%%-------------------------------------------------------------------
getChannal_keys(_Socket)->
    {ok, {IP, _Port}} = inet:peername(_Socket),
    F = integer_to_list(_Port), 
    case IP of
	{A, B, C, D} ->
	    lists:concat(["tcp_", A, "_", B, "_", C, "_", D, "_", F]);	
	Str when is_list(Str) ->
	    Str;
	_ ->
	    []
    end.


%%-------------------------------------------------------------------
%%接收CCAD发送过来的数据
transfer_Data(Data) ->
    dispatch(Data, []).


%%transfer_Data(Socket, Data) ->
%%    New_data = [Data|New_data],
%%    case length(New_data) >= 10#1024 of
%%	true ->	    
%%            Bin = zlib:gunzip(lists:reverse(New_Data)),	    
%%	    dispatch(Bin, []),
%%	    transfer_data(Socket, []);
%%	false ->
%%	    transfer_data(Socket, New_data)
%%    end.		

dispatch(Data, Key) ->
    case ccau_client_mgr:getNextClient(Key) of
	[Record] ->
	    Next = Record#clientinfo.id,	 
	    Pid = Record#clientinfo.pid,

	    gen_server:cast(Pid, {data, Data}),

	    dispatch(Data, Next);
	[] -> ok
    end.

   

