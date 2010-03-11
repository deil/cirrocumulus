-module(script_server).
-compile(export_all).

-define(Ruby, "/opt/ruby18/bin/ruby").

loop(Cirrocumulus) ->
    io:format("ScriptServer: started~n", []),
    Port = open_port({spawn, ?Ruby ++ " scripts/vps_monitor.rb"}, [{packet, 4}, nouse_stdio, exit_status, binary]),
    loop(Cirrocumulus, Port).

loop(Cirrocumulus, Port) ->
    Self = self(),
    receive
	stop ->
	    io:format("ScriptServer: stop~n", []);
	    
	{Port, {data, Data}} ->
	    {result, Text} = binary_to_term(Data),
	    io:format("ScriptServer: <- ~s~n", [Text]),
	    Cirrocumulus ! {Self, reply, Text},
	    loop(Cirrocumulus, Port);
	    
	{process, Data} ->
	    io:format("ScriptServer: -> ~s~n", [Data]),
	    port_command(Port, term_to_binary({echo, Data})),
	    loop(Cirrocumulus, Port)
    end.

