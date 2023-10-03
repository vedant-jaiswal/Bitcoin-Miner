-module(miningserver).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([triggerMiningOnServer/1, runDistributedMining/1]).
-define(SERVER, ?MODULE).
-record(test_state, {}).

%% The start_link enables the server to start receiving calls from the client through RPC i.e. Remote Procedure Call
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #test_state{}}.

%% Once called, this function has to call the runDistributedMining function
%% and essentially start the required task of mining bitcoins.
handle_call({triggerMiningOnServer, NumberOfLeadingZeroes}, _From, State) ->
  {reply, {miningserver:runDistributedMining(NumberOfLeadingZeroes)},State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Once the client calls the triggerMiningOnServer function with the required number of leading zeroes,
%% the below function handles it.
triggerMiningOnServer(NumberOfLeadingZeroes) ->
  gen_server:call({global,?MODULE},{triggerMiningOnServer, NumberOfLeadingZeroes}).

runDistributedMining(NumberOfLeadingZeroes) ->
  bitcoinminer:getCoins(NumberOfLeadingZeroes).