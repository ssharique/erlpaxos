%% Author: noah
%% Created: 2010-10-27
%% Description: TODO: Add description to client
-module(client).

%%
%% Include files
%%
-include("config.hrl").

%%
%% Exported Functions
%%
-export([start_link/1, propose/1]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(client_state, {proposers,
						n,
						r,
						w,
						request_id_prefix = 0,
						request_records = [],
		 				response_records = []
}).

-behaviour(gen_server).

%%
%% API Functions
%%
propose(Value) ->
	gen_server:call(?MODULE, Value).
	
start_link(State) ->
    gen_server:start({local, ?MODULE}, ?MODULE, State, []).

init({Proposers, N, R, W}) ->
    {ok, #client_state{proposers = Proposers, n = N, r = R, w = W}}.

handle_call(Request, _From, State) ->
	io:format("Client::Call Received Request: ~p~n", [Request]),
	
	Proposers = State#client_state.proposers,
	Id = State#client_state.request_id_prefix,
	RequestId = get_request_id(Id),
	
	Count = erlang:length(Proposers),
	Rand = random:uniform(Count),
	Prop = lists:nth(Rand, Proposers),
	
	RequestRecord = State#client_state.request_records,
	NewRequestRecord = create_request_timeout(RequestId, RequestRecord),
	
	Result = gen_server:abcast([Prop], proposer, {start_new_proposal, {RequestId, Request, node()}}),
	{reply, Result, State#client_state{request_id_prefix = Id + 1, request_records = NewRequestRecord}}.

handle_cast(Msg, State) ->
	io:format("Client::Cast Received Msg: ~p~n", [Msg]),
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
receive_msg({learn, RequstId, Node}, State) ->
	RequestRecord = State#client_state.request_records,
	ResponseRecords = State#client_state.response_records,
	
	W = State#client_state.w,
	
	NewResponseRecords = [{RequstId, Node} | ResponseRecords],
	
	Requests = proplists:get_all_values(RequstId, NewResponseRecords),
	Count = erlang:length(Requests),
	
%%	if 
%%		(Count >= W) ->
			
%%	end,
	
	State.
	
get_request_id(Id) ->
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	Id + MegaSecs + Secs + MicroSecs.
	
	
create_request_timeout(RequestId, RequestRecord) ->
	{ok, Tref} = timer:apply_after(?REQUEST_WAIT_TIMEOUT, gen_server, cast, [?MODULE, {request_timeout, RequestId}]), 
	[{RequestId, Tref} | RequestRecord].
