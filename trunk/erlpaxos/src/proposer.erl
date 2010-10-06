%% Author: noah
%% Created: 2010-10-2
%% Description: TODO: Add description to proposer
-module(proposer).

-author('Noah.Shen87@gmail.com').

%%
%% Include files
%%
-include("options.hrl").
-include("config.hrl").

%%
%% Exported Functions
%%
-export([start_link/1, propose/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         

-record(proposer_state, {acceptors,
		 				proposer_id,
		 				phase1_records = []
}).

-behaviour(gen_server).

%%
%% API Functions
%%
start_link(State) ->
    gen_server:start({local, ?MODULE}, ?MODULE, State, []).
    
propose(Value) ->
	gen_server:cast(?MODULE, {start_new_proposal, Value}).

init(State) ->
	io:format("Proposer Init State: ~p~n", [State]),
    {ok, #proposer_state{acceptors = State, proposer_id = 0}}.

handle_call(_Request, _From, State) ->
	{reply, {}, State}.

handle_cast(Msg, State) ->
	io:format("Proposer Received Msg: ~p~n", [Msg]),
	io:format("Proposer State: ~p~n", [State]),
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

propose_with_id(Id, N, Value) ->
	gen_server:cast(?MODULE, {start_proposal, Id, N, Value}).
	
receive_msg({start_proposal, Id, N, Value}, State) ->
	NewPhase1_records = create_promises_timeout(Id, N + 100, Value, State#proposer_state.phase1_records),
	gen_server:abcast(State#proposer_state.acceptors, acceptor, {prepare, Id, N, node()}),
	State#proposer_state{phase1_records = NewPhase1_records};
	
receive_msg({start_new_proposal, Value}, State) ->
	Id = State#proposer_state.proposer_id,
	propose_with_id(Id, ?START_N, Value),
	State#proposer_state{proposer_id = Id + 1};
	

receive_msg({promise_timeout, Id, N, V}, State) ->
	Phase1_records = State#proposer_state.phase1_records,
	Waiting = is_waiting({Id, N, V}, Phase1_records),
	if
		Waiting == true -> 
			io:format("PRO::TIMEOUT! Not enough promises for Id:~p, N:~p, V:~p - RETRY!~n", [Id, N, V]),
			NewPhase1_records = remove_from_waiting(Id, N, Phase1_records),
			propose_with_id(Id, N, V);
		true -> 
			io:format("PRO::Instance Id:~p is not waiting for promises anymore~n", [Id]),
			NewPhase1_records = Phase1_records
	end,
	State#proposer_state{phase1_records = NewPhase1_records}.

remove_from_waiting(Id, N, Phase1_records) ->
	lists:filter(fun({Id2, N2, V2, Tref}) -> not({Id2, N2, V2, Tref} == {Id, N, V2, Tref}) end, Phase1_records).
	
is_waiting({Id, N, V}, Phase1_records) ->
	lists:any(fun({Id2, N2, V2, Tref}) -> {Id2, N2, V2, Tref} == {Id, N, V, Tref} end, Phase1_records).

create_promises_timeout(Id, N, V, Phase1_records) ->
	{ok, Tref} = timer:apply_after(?PROMISE_WAIT_TIMEOUT, gen_server, cast, [?MODULE, {promise_timeout, Id, N, V}]), 
	[{Id, N, V, Tref} | Phase1_records].
	