-module(account_monitor).
-compile(export_all).

supported_ontology() ->
	"cirrocumulus-account_monitor".

init(Cirrocumulus) ->
	eresye:start(account_monitor),
	eresye:add_rule(account_monitor, {account_monitor, test_rule}),
	loop(Cirrocumulus).

loop(Cirrocumulus) ->
	receive
		stop ->
	    io:format("Brain: stop~n", []);
	
		{Sender, get_ontology} ->
	    Sender ! {get_ontology, supported_ontology()},
	    loop(Cirrocumulus);
	
		Signal ->
	    loop(Cirrocumulus)
	end.

test_rule(Engine, {first}) ->
	io:format("account_monitor::test~n", []).
