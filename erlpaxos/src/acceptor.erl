%% Author: noah
%% Created: 2010-10-6
%% Description: TODO: Add description to acceptor
-module(acceptor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/1, prepare/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         

-record(acceptor_state, {proposers,
		 				accepted_records = []
}).

-behaviour(gen_server).


%%
%% API Functions
%%
start_link(State) ->
    gen_server:start({local, ?MODULE}, ?MODULE, State, []).

prepare(Msg) ->
	gen_server:cast(?MODULE, Msg).

init(State) ->
	io:format("Acceptor Init State: ~p~n", [State]),
    {ok, #acceptor_state{proposers = State}}.

handle_call(_Request, _From, State) ->
	{reply, {}, State}.

handle_cast(Msg, State) ->
	io:format("Acceptor Received Msg: ~p~n", [Msg]),
%%	io:format("Acceptor State: ~p~n", [State]),
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

receive_msg({prepare, Id, ProposedN, Node}, State) ->
	Accepted_records = State#acceptor_state.accepted_records,
	case get_instance(Id, Accepted_records) of 
		new_instance ->
			NewAccepted_records = [{Id, {ProposedN, no_value}} | Accepted_records],
			io:format("ACC::Promising for id:~p, to n:~p, no previous value accepted~n", [Id, ProposedN]),
			Response = {promise, Id, ProposedN, no_value, node()};
		{BiggestPromised, AcceptedValue} ->
			if
				(ProposedN > BiggestPromised) ->
					io:format("ACC::Promising for id:~p, to n:~p, value:~p~n", [Id, ProposedN, Accepted_records]),
					NewAccepted_records = update_records_after_promise(Id, ProposedN, Accepted_records),
					Response = {promise, Id, ProposedN, AcceptedValue, node()};
				true ->
					io:format("ACC::Rejecting to promise for id:~p n:~p, already promised to ~p~n", [Id, ProposedN, BiggestPromised]),
					NewAccepted_records = Accepted_records,
					Response = {reject, Id, BiggestPromised, node()}
			end
	end,
	gen_server:abcast([Node], proposer, Response),
	State#acceptor_state{accepted_records = NewAccepted_records}.

update_records_after_promise(Id, ProposedN, Accepted_records) ->
	lists:map(fun({Id2, {ProposedN2, V}}) -> 
			if 
				{Id2, {ProposedN2, V}} == {Id, {ProposedN2, V}} ->
					{Id, {ProposedN, V}};
				true ->
					{Id2, {ProposedN2, V}}
			end
		end, Accepted_records).

get_instance(_, []) ->
	new_instance;
get_instance(Id, [{Id, Data} | _]) ->
	Data;
get_instance(Id, [_ | RestRecord]) ->
	get_instance(Id, RestRecord).
	
	