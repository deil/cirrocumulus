-module(vu_manager).
-compile([export_all]).

-include("fipa_message.hrl").

init(Cirrocumulus) ->
	eresye:start(vu_manager),
	eresye:add_rule(vu_manager, {vu_manager, start_vu}),
	eresye:add_rule(vu_manager, {vu_manager, halt_vu}),
	eresye:add_rule(vu_manager, {vu_manager, raw_reboot_vu}),
	eresye:add_rule(vu_manager, {vu_manager, reboot_vu}),
	MgrScript = spawn(script_server, init, [self(), "vu_manager.rb"]),
	MonScript = spawn(script_server, init, [self(), "vu_monitor.rb"]),
	logger:start(brain),
	loop(Cirrocumulus, MgrScript, MonScript).

loop(Cirrocumulus, MgrScript, MonScript) ->
	Self = self(),
	receive
		stop ->
			MgrScript ! stop,
			MonScript ! stop,
			logger:log(brain, "stop");
	
		{Sender, get_ontology} ->
	    	Sender ! {get_ontology, supported_ontology()},
	    	loop(Cirrocumulus, MgrScript, MonScript);
		
		%% incoming fact
		{assert_fact, Fact} ->
			case eresye:query_kb(vu_manager, Fact) of
				[] ->
					logger:log(brain, io_lib:format("received fact: ~p", [Fact])),
					eresye:assert(vu_manager, Fact);
				
				Whatever -> ok
			end,
	    loop(Cirrocumulus, MgrScript, MonScript);
			
		{retract_fact, Fact} ->
			case eresye:query_kb(vu_manager, Fact) of
				[] -> ok;
					
				Whatever ->
					logger:log(brain, io_lib:format("retracted fact: ~p", [Fact])),
					eresye:retract(vu_manager, Fact)
			end,
	    loop(Cirrocumulus, MgrScript, MonScript);
			
		%% passes incoming message to our knowledge base
		{process, Message} ->
			try
				Msg = list_to_tuple(lists:flatten([list_to_atom(Message#fipa_message.act), Message#fipa_message.content])),
				logger:log(brain, io_lib:format("processing fact {~p}", [Msg])),
				eresye:assert(vu_manager, Msg)
			catch
				_:Reason ->
					false
			end,
	    loop(Cirrocumulus, MgrScript, MonScript);

		%% processes a concrete action from current knowledge base rule
		{do, reboot_vu, VuId} ->
			logger:log(brain, io_lib:format("calling reboot_vu(~p)", [VuId])),
			case has_vu(VuId, MgrScript) of
				true ->
					logger:log(brain, io_lib:format("VU is running on me, rebooting", []));
				
				_ -> false
			end,
			loop(Cirrocumulus, MgrScript, MonScript);

		Signal ->
	    loop(Cirrocumulus, MgrScript, MonScript)
	end.

%%
%% functions
%%
supported_ontology() ->
	"cirrocumulus-vu_manager".

has_vu(VuId, MgrScript) ->
	EncodedVuId = list_to_binary(VuId),
	MgrScript ! {process, {has_vu, EncodedVuId}},
	receive
		{MgrScript, reply, {has_vu_reply, EncodedVuId, Result}} ->
			Result
	end.

%%
%% knowledge base definition
%%

raw_reboot_vu(Engine, {request, reboot_vu, VuId}) ->
	eresye:retract(Engine, {reboot_vu, VuId}),
	eresye:assert(Engine, {request_to, reboot_vu, list_to_binary(atom_to_list(VuId))}).

reboot_vu(Engine, {request_to, reboot_vu, VuId}, {has_vu_running, VuId}) ->
	eresye:retract(Engine, {reboot_vu, VuId}),
	logger:log(brain, io_lib:format("reboot_vu ~p", [VuId])).
	%%vu_manager_proc ! {do, reboot_vu, atom_to_list(VuId)},

halt_vu(Engine, {request, halt_vu, VuId}) ->
	logger:log(brain, io_lib:format("halt_vu ~p", [VuId])),
	vu_manager_proc ! {do, halt_vu},
	eresye:retract(Engine, {reboot_vu, VuId}).

start_vu(Engine, {request, start_vu, VuId}) ->
	logger:log(brain, io_lib:format("start_vu ~p", [VuId])),
	vu_manager_proc ! {do, start_vu},
	eresye:retract(Engine, {reboot_vu, VuId}).
