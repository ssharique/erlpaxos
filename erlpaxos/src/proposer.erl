%% Author: noah
%% Created: 2010-10-2
%% Description: TODO: Add description to proposer
-module(proposer).

-author('Noah.Shen87@gmail.com').

%%
%% Include files
%%
-include("options.hrl").

%%
%% Exported Functions
%%
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         

%%
%% API Functions
%%



%%
%% Local Functions
%%
start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).


init(State) ->
	io:format("State: ~p~n", [State]),
    {ok, [{accetpor, State}]}.

handle_call(_Request, _From, State) ->
	{reply, {}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

