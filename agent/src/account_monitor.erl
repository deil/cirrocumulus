-module(account_monitor).
-compile(export_all).

init(Cirrocumulus) ->
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

supported_ontology() ->
    "cirrocumulus-account_monitor".
