-module(vu_manager).
-compile([export_all]).

-include("fipa_message.hrl").

%% supported ontology by this brain
supported_ontology() ->
	"cirrocumulus-vu_manager".

loop(Cirrocumulus) ->
	receive
		stop ->
	    io:format("Brain: stop~n", []);
	
		{Sender, get_ontology} ->
	    Sender ! {get_ontology, supported_ontology()},
	    loop(Cirrocumulus);
			
		{process, Message} ->
			Msg = list_to_tuple(lists:flatten([list_to_atom(Message#fipa_message.act), Message#fipa_message.content])),
			eresye:assert(vu_manager, Msg),
	    loop(Cirrocumulus);
	
		Signal ->
	    loop(Cirrocumulus)
	end.

init(Cirrocumulus) ->
	eresye:start(vu_manager),
	eresye:add_rule(vu_manager, {vu_manager, start_vu}),
	eresye:add_rule(vu_manager, {vu_manager, halt_vu}),
	eresye:add_rule(vu_manager, {vu_manager, reboot_vu}),
	logger:start(brain),
	loop(Cirrocumulus).

%%
%% knowledge base definition
%%

reboot_vu(Engine, {request, reboot_vu, VuId}) ->
	logger:log(brain, io_lib:format("reboot_vu ~p", [VuId])),
	eresye:retract(Engine, {reboot_vu, VuId}).

halt_vu(Engine, {request, halt_vu, VuId}) ->
	logger:log(brain, io_lib:format("halt_vu ~p", [VuId])),
	eresye:retract(Engine, {reboot_vu, VuId}).

start_vu(Engine, {request, start_vu, VuId}) ->
	logger:log(brain, io_lib:format("start_vu ~p", [VuId])),
	eresye:retract(Engine, {reboot_vu, VuId}).
