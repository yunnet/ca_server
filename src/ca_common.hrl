%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 22 Oct 2013 by  <>
%%%-------------------------------------------------------------------
-record(clientinfo, {id,              %% key
                     socket,          %% socket
		     pid,             %% thread
                     simno = "",           
                     channel = 0               
		     }).
