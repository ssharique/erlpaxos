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
						majority,
		 				proposer_id,
		 				instance_records = [],
		 				promise_records = []
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
	Length = erlang:length(State),
    {ok, #proposer_state{acceptors = State, proposer_id = 0, majority = (Length div 2) + 1}}.

handle_call(_Request, _From, State) ->
	{reply, {}, State}.

handle_cast(Msg, State) ->
	io:format("Proposer Received Msg: ~p~n", [Msg]),
%%	io:format("Proposer State: ~p~n", [State]),
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

propose_with_id(Id, N, ClientValue) ->
	gen_server:cast(?MODULE, {start_proposal, Id, N, ClientValue}).
	
receive_msg({start_proposal, Id, N, ClientValue}, State) ->
	NewInstance_records = create_promises_timeout(Id, N, ClientValue, State#proposer_state.instance_records),
	gen_server:abcast(State#proposer_state.acceptors, acceptor, {prepare, Id, N, node()}),
	State#proposer_state{instance_records = NewInstance_records};
	
receive_msg({start_new_proposal, ClientValue}, State) ->
	Id = State#proposer_state.proposer_id,
	propose_with_id(Id, ?START_N, ClientValue),
	State#proposer_state{proposer_id = Id + 1};
	

receive_msg({promise_timeout, Id, N, V}, State) ->
	Instance_records = State#proposer_state.instance_records,
	PromiseRecords = State#proposer_state.promise_records,
	Waiting = proplists:is_defined(Id, Instance_records),
	if
		Waiting == true -> 
			io:format("PRO::TIMEOUT! Not enough promises for Id:~p, N:~p, V:~p - RETRY!~n", [Id, N, V]),
			NewInstance_records = proplists:delete(Id, Instance_records),
			NewPromiseRecords = proplists:delete(Id, PromiseRecords),
			propose_with_id(Id, N + ?STEP_N, V);
		true -> 
			io:format("PRO::Instance Id:~p is not waiting for promises anymore~n", [Id]),
			NewPromiseRecords = PromiseRecords,
			NewInstance_records = Instance_records
	end,
	State#proposer_state{instance_records = NewInstance_records, promise_records = NewPromiseRecords};

receive_msg({promise, Id, PromiseN, AcceptedValue, Node}, State) ->
	Instance_records = State#proposer_state.instance_records,
	PromiseRecords = State#proposer_state.promise_records,
	Majority = State#proposer_state.majority,
	case proplists:get_value(Id, Instance_records) of
		undefined ->
			NewPromiseRecords = PromiseRecords,
			NewInstance_records = Instance_records;
		{_N, _ClientValue, Tref} ->
			NewPromiseRecords = [{Id, {PromiseN, AcceptedValue, Node}} | PromiseRecords],
			Count = erlang:length(proplists:get_all_values(Id, NewPromiseRecords)),
			if
				(Count >= Majority) ->
					io:format("PRO::Got a majority of promises for id:~p N:~p~n", [Id, PromiseN]),
					NewInstance_records = proplists:delete(Id, Instance_records),
					timer:cancel(Tref);
					%% TODO
				true ->
					io:format("PRO::Not enough promises so far for id:~p~n", [Id]),
					NewInstance_records = Instance_records
			end
	end,
	State#proposer_state{instance_records = NewInstance_records, promise_records = NewPromiseRecords}.
	

create_promises_timeout(Id, N, ClientValue, Instance_records) ->
	{ok, Tref} = timer:apply_after(?PROMISE_WAIT_TIMEOUT, gen_server, cast, [?MODULE, {promise_timeout, Id, N, ClientValue}]), 
	[{Id, {N, ClientValue, Tref}} | Instance_records].
	