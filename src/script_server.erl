-module(script_server).
-compile(export_all).

%%-define(Ruby, "/opt/ruby18/bin/ruby").
-define(Ruby, "/usr/bin/ruby").

init(Master, Script) ->
	logger:start(list_to_atom(Script)),
	logger:log(list_to_atom(Script), "started"),
	Port = open_port({spawn, ?Ruby ++ " ../scripts/" ++ Script}, [{packet, 4}, nouse_stdio, exit_status, binary]),
	loop(Master, Script, Port).

loop(Master, Script, Port) ->
	Self = self(),
	receive
		stop ->
			logger:log(list_to_atom(Script), "stop~n");
	    
		{Port, {data, Data}} ->
			%%logger:log(list_to_atom(Script), io_lib:format("~p", [Data])),
			Message = binary_to_term(Data),
			parse_reply(Message, Master, Script, Self),					
			loop(Master, Script, Port);
	    
		{process, Data} ->
			logger:log(list_to_atom(Script), io_lib:format("sending: ~p", [Data])),
	    	port_command(Port, term_to_binary(Data)),
	    	loop(Master, Script, Port)
	end.

parse_reply(Message, Master, Script, Self) ->
	logger:log(list_to_atom(Script), io_lib:format("received: ~p", [Message])),
	case Message of
		{assert, InfoMessage} ->
			Master ! {assert_fact, InfoMessage};
			
		{retract, InfoMessage} ->
			Master ! {retract_fact, InfoMessage};
			
		Other ->
			logger:log(list_to_atom(Script), io_lib:format("received: ~p", [Message])),
			Master ! {Self, reply, Message}
	end.	
