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
		 				instance_id = 0,
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
    {ok, #proposer_state{acceptors = State, majority = (Length div 2) + 1}}.

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
	Id = State#proposer_state.instance_id,
	propose_with_id(Id, ?START_N, ClientValue),
	State#proposer_state{instance_id = Id + 1};
	

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
		{_N, ClientValue, Tref} ->
			NewPromiseRecords2 = [{Id, {PromiseN, AcceptedValue, Node}} | PromiseRecords],
			Prom = proplists:get_all_values(Id, NewPromiseRecords2),
			Count = erlang:length(Prom),
			if
				(Count >= Majority) ->
					io:format("PRO::Got a majority of promises for id:~p N:~p~n", [Id, PromiseN]),
					
					NewInstance_records = remove_instance(Id, Instance_records, Tref),
					
					NewPromiseRecords = proplists:delete(Id, NewPromiseRecords2),
					{MaxPromN, Set} = get_values_set({0, []}, Prom),
					case pick_up_value(MaxPromN, Set) of
						{value_not_chosen, PromN, no_value} ->
							io:format("PRO::We promises with no previous value, sending accept v:~p for id:~p~n", [ClientValue, Id]),
							Response = {accept, Id, PromN, ClientValue, node()},
							gen_server:abcast(State#proposer_state.acceptors, acceptor, Response);
						{value_chosen, PromN, Value} ->
							io:format("PRO::My value:~p has been choosen in instance:~p N:~p~n", [Value, Id, PromN]),
							Response = {accept, Id, PromN, Value, node()},
							gen_server:abcast(State#proposer_state.acceptors, acceptor, Response);
						{value_not_chosen, PromN, Set} ->
							io:format("PRO::Somebody else value is best candidate, sending accept v:~p for id:~p~n", [Set, Id]),
							io:format("PRO::... still need to propose ~p - START OVER!~n", [ClientValue]),
							propose_with_id(Id, PromN + ?STEP_N, ClientValue)
					end;
					
				true ->
					io:format("PRO::Not enough promises so far for id:~p~n", [Id]),
					NewPromiseRecords = NewPromiseRecords2,
					NewInstance_records = Instance_records
			end
	end,
	State#proposer_state{instance_records = NewInstance_records, promise_records = NewPromiseRecords};
	
receive_msg({old_instance, Id, _ProposedN, _Node}, State) ->
	Instance_records = State#proposer_state.instance_records,
	{Id, {_N, ClientValue, Tref}} = proplists:get_value(Id, Instance_records),
	NewInstance_records = remove_instance(Id, Instance_records, Tref),
	timer:cancel(Tref),
	propose(ClientValue),
	State#proposer_state{instance_records = NewInstance_records};

receive_msg({accepted, Id, _N, _Value, _Node}, State) ->
	OldId = State#proposer_state.instance_id,
	if
		(Id >= OldId) ->
			NewId = Id + 1;
		true ->
			NewId = OldId
	end,
	io:format("PRO::new id:~p~n", [NewId]),
	State#proposer_state{instance_id = NewId}.
	

pick_up_value(PromN, Set) ->
	Count = erlang:length(Set),
	if 
		(Count == 1) ->
			Value = lists:nth(1, Set),
			if
				(Value == no_value) ->
					{value_not_chosen, PromN, no_value};
				true ->
					{value_chosen, PromN, Value}
			end;
		true ->
			{value_not_chosen, PromN, Set}
	end.
	
get_values_set(ValueSet, []) ->
	ValueSet;
get_values_set({MaxPromN, Set}, [{PromiseN, AcceptedValue, _Node} | RestProm]) ->
	if 
		(PromiseN > MaxPromN) ->
			get_values_set({PromiseN, [AcceptedValue]}, RestProm);
		(PromiseN == MaxPromN) ->
			IsDup = lists:member(AcceptedValue, Set),
			if 
				IsDup == false -> 
					get_values_set({PromiseN, [AcceptedValue | Set]}, RestProm);
				true ->
					get_values_set({PromiseN, [AcceptedValue]}, RestProm)
			end;
		true ->
			get_values_set({MaxPromN, Set}, RestProm)
	end;
	
get_values_set(ValueSet, [_ | RestProm]) ->
	get_values_set(ValueSet, RestProm).
	
remove_instance(Id, Instance_records, Tref) ->
	NewInstance_records = proplists:delete(Id, Instance_records),
	timer:cancel(Tref),
	NewInstance_records.
					
create_promises_timeout(Id, N, ClientValue, Instance_records) ->
	{ok, Tref} = timer:apply_after(?PROMISE_WAIT_TIMEOUT, gen_server, cast, [?MODULE, {promise_timeout, Id, N, ClientValue}]), 
	[{Id, {N, ClientValue, Tref}} | Instance_records].
	