-module(account_monitor).
-compile(export_all).

supported_ontology() ->
	"cirrocumulus-account_monitor".

init(Cirrocumulus) ->
	eresye:start(account_monitor),
	eresye:add_rule(account_monitor, {account_monitor, new_account}),
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

%%
%% knowledge base definition
%%

new_account(Engine, {inform, new_account, AccountId}) ->
	eresye:retract(Engine, {inform, new_account, AccountId}),
	logger:log(brain, io_lib:format("new account id=~p", [AccountId])).
