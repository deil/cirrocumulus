-module(cirrocumulus).

-compile(export_all).

start() ->
	spawn(?MODULE, init, []).

stop(AgentPid) ->
	AgentPid ! stop.

init() ->
	Script = "xen_node_monitor",
	Brain = spawn(list_to_atom(Script), init, [self()]),
	register(list_to_atom(Script ++ "_proc"), Brain),
	MessageBus = spawn(message_bus, init, [self(), Brain, Script]),
	loop(Brain, MessageBus).

loop(Brain, MessageBus) ->
	receive
		stop ->
			Brain ! stop,
			MessageBus ! stop;

		{MessageBus, receive_message, Message} ->
			Brain ! {process, Message},
			loop(Brain, MessageBus);
	    
		_ ->
			loop(Brain, MessageBus)
	end.
