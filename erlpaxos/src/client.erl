%% Author: noah
%% Created: 2010-10-27
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/1, propose/1]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(client_state, {proposers
}).

-behaviour(gen_server).

%%
%% API Functions
%%
propose(Value) ->
	gen_server:call(?MODULE, Value).
	
start_link(State) ->
    gen_server:start({local, ?MODULE}, ?MODULE, State, []).

init(State) ->
    {ok, #client_state{proposers = State}}.

handle_call(Request, _From, State) ->
	io:format("Learner Call Received Request: ~p~n", [Request]),
	
	Proposers = State#client_state.proposers,
	Count = erlang:length(Proposers),
	io:format("Count: ~p~n", [Count]),
	Rand = random:uniform(Count),
	io:format("Rand: ~p~n", [Rand]),
	Prop = lists:nth(Rand, Proposers),
	Result = gen_server:abcast([Prop], proposer, {start_new_proposal, {Request, node()}}),
	{reply, Result, State}.

handle_cast(Msg, State) ->
	io:format("Learner Cast Received Msg: ~p~n", [Msg]),
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
receive_msg({learner, Id, Node}, State) ->
	State.
