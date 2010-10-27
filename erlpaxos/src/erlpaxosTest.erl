%% Author: noah
%% Created: 2010-10-2
%% Description: TODO: Add description to erlpaxosTest
-module(erlpaxosTest).

-compile(export_all).
%%
%% Include files
%%
-include("options.hrl").
		 
%%
%% Exported Functions
%%
%% -export([]).

%%
%% API Functions
%%
start() ->
	{ok,Term} = file:consult("options.opt"),
	{ok, State} = parseOpts(Term),
	R1 = proposer:start_link(State#paxosOpts.acceptors),
	R2 = acceptor:start_link(State#paxosOpts.proposers),
	R3 = client:start_link(State#paxosOpts.proposers),
	{R1, R2, R3}.
	
	
stop() ->
    void.

propose_test() ->
	client:propose("Test").

%%
%% Local Functions
%%

	
parseOpts(Term) ->
	parseOpts(Term, #paxosOpts{}).

parseOpts([], State) ->
	{ok, State};

parseOpts([{proposer, Nodes} | T], State) ->
	parseOpts(T, State#paxosOpts{proposers = Nodes});
	
parseOpts([{acceptor, Nodes} | T], State) ->
	parseOpts(T, State#paxosOpts{acceptors = Nodes});

parseOpts([{learner, Nodes} | T], State) ->
	parseOpts(T, State#paxosOpts{learners = Nodes}).
	
	