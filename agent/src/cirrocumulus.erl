-module(cirrocumulus).

-compile(export_all).

start() ->
	spawn(?MODULE, init, []).

stop(AgentPid) ->
	AgentPid ! stop.

init() ->
	Script = "vu_manager",
	Brain = spawn(list_to_atom(Script), init, [self()]),
	MessageBus = spawn(message_bus, init, [self(), Brain, Script]),
	ScriptServer = spawn(script_server, init, [self(), Script ++ ".rb"]),
	loop(Brain, MessageBus, ScriptServer).

loop(Brain, MessageBus, ScriptServer) ->
	receive
		stop ->
			Brain ! stop,
			ScriptServer ! stop,
			MessageBus ! stop;

		{MessageBus, receive_message, Message} ->
			Brain ! {process, Message},
			%%ScriptServer ! {process, Text},
			loop(Brain, MessageBus, ScriptServer);
	    
		{ScriptServer, reply, Text} ->
	    MessageBus ! {message, Text},
	    loop(Brain, MessageBus, ScriptServer);

		_ ->
			loop(Brain, MessageBus, ScriptServer)
	end.
