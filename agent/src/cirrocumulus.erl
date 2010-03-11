-module(cirrocumulus).

-compile(export_all).

start() ->
    spawn(?MODULE, init, []).

stop(AgentPid) ->
    AgentPid ! stop.

start_script() ->
    spawn(script_server, loop, [self()]).

init() ->
    MessageBus = spawn(message_bus, init, [self()]),
    ScriptServer = spawn(script_server, loop, [self()]),
    loop(MessageBus, ScriptServer).

loop(MessageBus, ScriptServer) ->
    receive
        stop ->
	    ScriptServer ! stop,
	    MessageBus ! stop;

        _ ->
    	    loop(MessageBus, ScriptServer)
    end.
