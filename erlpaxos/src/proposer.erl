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
-export([start_link/1, propose/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         

-record(proposer_state,
        {acceptors,
		 proposer_id}).
		 
%%
%% API Functions
%%
start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).
    
propose(Value) ->
	gen_server:cast(?MODULE, {start_proposal, Value}).


init(State) ->
	io:format("State: ~p~n", [State]),
    {ok, #proposer_state{acceptors = State, proposer_id = 0}}.

handle_call(_Request, _From, State) ->
	{reply, {}, State}.

handle_cast(Msg, State) ->
%%	io:format("Msg: ~p~n", [Msg]),
	receive_msg(Msg, State),
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Local Functions
%%

receive_msg({start_proposal, Value}, State) ->
	io:format("Value: ~p~n", [Value]),
	
	io:format("State: ~p~n", [State]),
	gen_server:abcast(State#proposer_state.acceptors, aceceptor, Value).
	

	