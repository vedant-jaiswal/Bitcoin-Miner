-module(bitcoinminer).

-import(binary, [decode_unsigned/1]).
-import(crypto, [hash/1]).
-export([getRandomStringFromCrypto/0, performMiningRecursively/3, getCoins/1, miner/0, collectCoins/0]).
-define(CommonPrefix, "ppasumarty").
-define(PrefixAndKeySeparator, ";").
-define(RecursionKillPoint, 64).

performMiningRecursively(NumberOfLeadingZeroes, Counter, BossActor) ->
%%	Create a random string at each iteration. Append this string with required prefix.
	RandomString = bitcoinminer:getRandomStringFromCrypto(),
	StringToBeHashed = string:concat(?CommonPrefix, string:concat(?PrefixAndKeySeparator, RandomString)),
%%	Find the SHA 256 hash of the created input string
	HashAsInteger = binary:decode_unsigned(crypto:hash(sha256,StringToBeHashed)),
	HashAsString = io_lib:format("~64.16.0b", [HashAsInteger]),
	LeadingZeroesString = io_lib:format("~*..0b", [NumberOfLeadingZeroes, 0]),
%%	If the generated Hash starts with the number of required leading zeroes, it is sent to the Boss Actor
%%	through message passing. The Boss Actor would then output it to the console.
	case (string:str(HashAsString, LeadingZeroesString) == 1) of
%%	If the actor has finished mining its target, the recursion ends and control is returned to the parent function.
%%	Otherwise the recursion continues so as to mine further coins.
	    true -> 
			NewCounter = Counter + 1,
			BossActor ! {StringToBeHashed, HashAsString},
			if
				NewCounter < ?RecursionKillPoint ->
					bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, NewCounter, BossActor);
				true ->
					ok
			end;
	    false ->
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, Counter, BossActor)
  	end.

getRandomStringFromCrypto() -> base64:encode_to_string(crypto:strong_rand_bytes(6)).

getCoins(NumberOfLeadingZeroes) ->
	io:fwrite("Coin collection started ~n"),
%%	Open the file and write "" to it. This essentially clears the contents of the file
%%	The file would then be ready for writing new statistics.
	{ok, FilePointer} = file:open("MiningStatistics.txt", [write]),
	io:format(FilePointer, "", []),
	file:close("MiningStatistics.txt"),
	BossActor = spawn(fun bitcoinminer:collectCoins/0),
	NumberOfActors = 8,
	lists:foldl(
		fun(_, _) -> 
			ChildActorPID = spawn(fun bitcoinminer:miner/0),
			ChildActorPID ! {ChildActorPID, NumberOfLeadingZeroes, BossActor}
		end, 
		[], 
		lists:seq(1, NumberOfActors)
	).

miner() ->
	receive
		{From, NumberOfLeadingZeroes, BossActor} ->
			{ok, FilePointer} = file:open("MiningStatistics.txt", [append]),
			statistics(runtime),
			statistics(wall_clock),
			bitcoinminer:performMiningRecursively(NumberOfLeadingZeroes, 0, BossActor),
			{_,CPUTimeTakenSinceLastCall} = statistics(runtime),
			{_,RealTimeTakenSinceLastCall} = statistics(wall_clock),
			io:format(FilePointer, "Statistics of Actor: ~p ~n", [From]),
			io:format(FilePointer, "CPU time ~p Milliseconds ~n", [CPUTimeTakenSinceLastCall]),
			io:format(FilePointer, "Real time ~p Milliseconds ~n", [RealTimeTakenSinceLastCall]),
			io:format(FilePointer, "**************************** ~n", [])
	end.

collectCoins() ->
%%	Whenever a child actor finds a valid bitcoin, it is passed in a message to the Boss actor
%%	It is then the Boss Actor's task to output it to the console
	receive
		{StringToBeHashed, HashedString} ->
			io:fwrite("~p	~p~n", [StringToBeHashed, HashedString]),
			collectCoins()
	end.