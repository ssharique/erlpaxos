%% Author: noah
%% Created: 2010-10-26
%% Description: TODO: Add description to learner
-module(learner).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
-behaviour(gen_server).

%%
%% API Functions
%%
start_link(State) ->
    gen_server:start({local, ?MODULE}, ?MODULE, State, []).

init(State) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
	{reply, {}, State}.

handle_cast(Msg, State) ->
	io:format("Learner::Cast Received Msg: ~p~n", [Msg]),
	{noreply, receive_msg(Msg, State)}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Local Functions
%%
receive_msg({learn, Id, {RequestId, Value, ClientNode}, Node}, State) ->
	gen_server:abcast([ClientNode], client, {learn, RequestId, node()}),
	State.
