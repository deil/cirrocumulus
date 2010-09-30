%% Author: deil
%% Created: 22.08.2010
%% Description: TODO: Add description to xen_node_monitor
-module(xen_node_monitor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 supported_ontology/0, init/1,
		 aoe_up/2, aoe_up_for_active_raid/3,
		 aoe_down/3, aoe_down_for_active_raid/4,
		 raid_active/2
		]).

%%
%% API Functions
%%
supported_ontology() ->
	"cirrocumulus-xen_node_monitor".

init(Cirrocumulus) ->
	eresye:start(xen_node_monitor),
	eresye:add_rule(xen_node_monitor, {xen_node_monitor, aoe_up_for_active_raid}),
	eresye:add_rule(xen_node_monitor, {xen_node_monitor, aoe_up}),
	eresye:add_rule(xen_node_monitor, {xen_node_monitor, aoe_down_for_active_raid}),
	eresye:add_rule(xen_node_monitor, {xen_node_monitor, aoe_down}),
	eresye:add_rule(xen_node_monitor, {xen_node_monitor, raid_active}),
	MonScript = spawn(script_server, init, [self(), "xen_node_monitor.rb"]),
	logger:start(brain),
	loop(Cirrocumulus, MonScript).

%%
%% Local Functions
%%
loop(Cirrocumulus, MonScript) ->
	receive
		stop ->
	    	io:format("Brain: stop~n", []);
	
		{Sender, get_ontology} ->
	    	Sender ! {get_ontology, supported_ontology()},
	    	loop(Cirrocumulus, MonScript);
		
		%% incoming fact
		{assert_fact, Fact} ->
			case eresye:query_kb(xen_node_monitor, Fact) of
				[] ->
					logger:log(brain, io_lib:format("received fact: ~p", [Fact])),
					eresye:assert(xen_node_monitor, Fact);
				
				_ -> ok
			end,
	    	loop(Cirrocumulus, MonScript);
			
		{retract_fact, Fact} ->
			case eresye:query_kb(xen_node_monitor, Fact) of
				[] -> ok;
					
				_ ->
					logger:log(brain, io_lib:format("retracted fact: ~p", [Fact])),
					eresye:retract(xen_node_monitor, Fact)
			end,
	    	loop(Cirrocumulus, MonScript);		
	
		_ ->
	    	loop(Cirrocumulus, MonScript)
	end.

%%
%% knowledge base definition
%%
aoe_up(Engine, {aoe_up, Major, Minor}) ->
	logger:log(brain, io_lib:format("aoe device appeared: e~p.~p", [Major, Minor])),
	eresye:retract(Engine, {aoe_up, Major, Minor}),
	eresye:assert(Engine, {is_aoe_up, Major, Minor}).

aoe_up_for_active_raid(Engine,
						{aoe_up, Major, Minor},
						{is_raid_active, Major, NumDevices}) ->
	logger:log(brain, io_lib:format("aoe device for md~p went up: e~p.~p", [Major, Major, Minor])),
	eresye:retract(Engine, {aoe_up, Major, Minor}),
	eresye:retract(Engine, {is_aoe_down, Major, Minor}),
	eresye:retract(Engine, {is_raid_active, Major, NumDevices}),
	eresye:assert(Engine, {is_raid_active, Major, NumDevices + 1}),
	eresye:assert(Engine, {is_aoe_up, Major, Minor}).

aoe_down(Engine, {aoe_down, Major, Minor}, {is_aoe_up, Major, Minor}) ->
	logger:log(brain, io_lib:format("aoe device went down: e~p.~p", [Major, Minor])),
	eresye:retract(Engine, {aoe_down, Major, Minor}),
	eresye:retract(Engine, {is_aoe_up, Major, Minor}),
	eresye:assert(Engine, {is_aoe_down, Major, Minor}).

aoe_down_for_active_raid(Engine,
						{aoe_down, Major, Minor},
						{is_aoe_up, Major, Minor},
						{is_raid_active, Major, NumDevices}) ->
	logger:log(brain, io_lib:format("aoe device for md~p went down: e~p.~p", [Major, Major, Minor])),
	eresye:retract(Engine, {aoe_down, Major, Minor}),
	eresye:retract(Engine, {is_aoe_up, Major, Minor}),
	eresye:retract(Engine, {is_raid_active, Major, NumDevices}),
	eresye:assert(Engine, {is_raid_active, Major, NumDevices - 1}),
	eresye:assert(Engine, {is_aoe_down, Major, Minor}).

raid_active(Engine, {raid_active, Major, NumDevices}) ->
	logger:log(brain, io_lib:format("active raid array: md~p", [Major])),
	eresye:assert(Engine, {is_raid_active, Major, NumDevices}).
