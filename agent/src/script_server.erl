-module(script_server).
-compile(export_all).

-define(Ruby, "/opt/ruby18/bin/ruby").

loop(MainServer) ->
    io:format("ScriptServer: started~n", []),
    Port = open_port({spawn, ?Ruby ++ " scripts/vps_monitor.rb"}, [{packet, 4}, nouse_stdio, exit_status, binary]),
    loop(MainServer, Port).

loop(MainServer, Port) ->
    receive
	stop ->
	    io:format("ScriptServer: stop~n", []);
	    
	{Port, {data, Data}} ->
	    {result, Text} = binary_to_term(Data),
	    io:format("ScriptServer: <- ~s~n", [Text]),
	    MainServer ! {MainServer, Text},
	    loop(MainServer, Port);
	    
	{send, Data} ->
	    io:format("ScriptServer: -> ~s~n", [Data]),
	    port_command(Port, term_to_binary({echo, Data})),
	    loop(MainServer, Port)
    end.

