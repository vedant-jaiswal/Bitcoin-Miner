-module(miningclient).

-export([triggerMiningOnServer/1]).

%% The task of the client is simple. Once a connection is created between the client and server nodes,
%% the client just has to trigger the mining activity on the server
triggerMiningOnServer(NumberOfLeadingZeroes)->
  rpc:call('myserver@10.20.166.52', miningserver, triggerMiningOnServer,[NumberOfLeadingZeroes]).
