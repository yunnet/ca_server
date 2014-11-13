%%%-------------------------------------------------------------------
%%% @author  <yunnet>
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 7 Sep 2014 by  <yunnet>
%%%-------------------------------------------------------------------
-module(ca_server).

-export([start/0, stop/0, restart/0, info/0]).

start()->
    dbg:start(),     % start dbg
    dbg:tracer(),    % start a simple tracer process
    dbg:p(all, c),   % trace calls (c) of that MFA for all processes.
    application:start(ca_server),
    ok.

stop()->
    dbg:stop(),
    application:stop(ca_server).

restart() ->
    case stop() of
    ok ->
        start();
    {error, {not_started,couch}} ->
        start();
    {error, Reason} ->
        {error, Reason}
    end.


%%===================================================================
%%===================================================================
info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    io:format( "abormal termination:
                     ~n   Scheduler id:                         ~p
                     ~n   Num scheduler:                        ~p
                     ~n   Process count:                        ~p
                     ~n   Process limit:                        ~p
                     ~n   Memory used by erlang processes:      ~p
                     ~n   Memory allocated by erlang processes: ~p
                     ~n   The total amount of memory allocated: ~p
                     ~n",
                            [SchedId, SchedNum, ProcCount, ProcLimit,
                             ProcMemUsed, ProcMemAlloc, MemTot
                             ]),
      ok.

    
