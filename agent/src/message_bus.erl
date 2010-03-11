-module(message_bus).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-compile(export_all).

-define(Server, "89.223.109.31").
-define(Port, 5222).
-define(Hostname, "hc001.o1host.net").
-define(Chatroom, "cirrocumulus@conference.o1host.net").

init(Cirrocumulus) ->
    application:start(exmpp),
    
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    
    %% Create XMPP ID (Session Key):
    MyJID = exmpp_jid:make(hostname(), ?Hostname, "Cirrocumulus"),
    io:format("MessageBuss: logging as ~s@~s~n", [hostname(), ?Hostname]),
    
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, hostname()),
    
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, ?Server, ?Port),
    session(MySession, MyJID),
    loop(MySession, Cirrocumulus).

hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

session(MySession, _MyJID) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("Register~n",[]),
	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, hostname()),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,

    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Cirrocumulus")),

    %% Join groupchat
    groupchat(MySession).

groupchat(MySession) ->
    Packet = exmpp_xml:set_attribute(#xmlel{name = 'presence'}, to, ?Chatroom ++ "/" ++ hostname()),
    exmpp_session:send_packet(MySession, Packet).

%% Process exmpp packet:
loop(MySession, Cirrocumulus) ->
    Self = self(),

    receive
        stop ->
    	    io:format("MessageBus: stop~n"),
            exmpp_session:stop(MySession);

        {message, Text} ->
    	    send_message(MySession, Text),
    	    loop(MySession, Cirrocumulus);

        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message, raw_packet=Packet} ->
    	    %%io:format("--> ~s~n", [exmpp_xml:document_to_binary(Packet)]),
    	    From = exmpp_xml:get_attribute(Packet, from, undefined),
    	    HasBody = exmpp_xml:has_element(Packet, body),
    	    MyMessage = message_from_self(From),
    	    ShouldProcess = HasBody and not MyMessage,
    	    case ShouldProcess of
    		true ->
    		    BodyElem = exmpp_xml:get_element(Packet, body),
    		    Body = exmpp_xml:get_cdata(BodyElem),
    		    io:format("MessageBus: -> ~s:~s~n", [From, Body]),
    		    Cirrocumulus ! {Self, message, Body};
    		    %%process_message(MySession, From, Body);
    		_ -> false
    	    end,
            loop(MySession, Cirrocumulus);
            
        Record ->
            %%io:format("~p~n", [Record]),
            loop(MySession, Cirrocumulus)
    end.
    
message_from_self(From) ->
    Res = regexp:match(binary_to_list(From), hostname()),
    case Res of
	nomatch -> false;
	_ -> true 
    end.

send_message(MySession, Text) ->
    Msg = #xmlel{name = "message"},
    Msg1 = exmpp_xml:set_attribute(Msg, type, <<"groupchat">>),
    Msg2 = exmpp_xml:set_attribute(Msg1, to, ?Chatroom),
    Body = #xmlel{name = "body"},
    Body1 = exmpp_xml:set_cdata(Body, Text),
    Packet = exmpp_xml:append_child(Msg2, Body1),
    exmpp_session:send_packet(MySession, Packet).
    
process_message(MySession, From, Text) ->
    send_message(MySession, "сам хуй!").

%% Send the same packet back for each message received
echo_packet(MySession, Packet) ->
    An1 = #xmlel{name = 'iq'},
    An2 = exmpp_xml:set_attribute(An1, type, <<"set">>),
    An3 = exmpp_xml:set_attribute(An2, to, "o1host.net"),
    An4 = exmpp_xml:set_attribute(An3, id, '666'),
    Cmd1 = #xmlel{ns = "http://jabber.org/protocol/commands", name = "command"},
    %%Cmd2 = exmpp_xml:declare_ns_here(Cmd1, "http://jabber.org/protocol/commands", none),
    Cmd3 = exmpp_xml:set_attribute(Cmd1, node, <<"http://jabber.org/protocol/admin#announce">>),
    X1 = #xmlel{ns = "jabber:x:data", name = "x"},
    %%X2 = exmpp_xml:declare_ns_here(X1, "jabber:x:data", none),
    X3 = exmpp_xml:set_attribute(X1, type, <<"submit">>),
    Field1 = exmpp_xml:element("field"),
    Field2 = exmpp_xml:set_attribute(Field1, type, <<"hidden">>),
    Field3 = exmpp_xml:set_attribute(Field2, var, <<"FORM_TYPE">>),
    Val1 = exmpp_xml:element("value"),
    Val2 = exmpp_xml:set_cdata(Val1, <<"http://jabber.org/protocol/admin">>),
    Field4 = exmpp_xml:append_child(Field3, Val2),
    Field5 = exmpp_xml:element("field"),
    Field6 = exmpp_xml:set_attribute(Field5, type, <<"text-single">>),
    Field7 = exmpp_xml:set_attribute(Field6, var, <<"subject">>),
    Val3 = exmpp_xml:element("value"),
    Val4 = exmpp_xml:set_cdata(Val3, <<"subj">>),
    Field8 = exmpp_xml:append_child(Field7, Val4),
    Field9 = exmpp_xml:element("field"),
    Field10 = exmpp_xml:set_attribute(Field9, type, <<"text-multi">>),
    Field11 = exmpp_xml:set_attribute(Field10, var, <<"body">>),
    Val5 = exmpp_xml:element("value"),
    Val6 = exmpp_xml:set_cdata(Val5, <<"body">>),
    Field12 = exmpp_xml:append_child(Field11, Val6),
    X4 = exmpp_xml:append_child(X3, Field4),
    X5 = exmpp_xml:append_child(X4, Field8),
    X6 = exmpp_xml:append_child(X5, Field12),
    Cmd4 = exmpp_xml:append_child(Cmd3, X6),
    An = exmpp_xml:append_child(An4, Cmd4),
    From = exmpp_xml:get_attribute(Packet, from, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, to, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, from, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, to, <<"cirrocumulus@conference.o1host.net">>),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, id),
    exmpp_session:send_packet(MySession, NewPacket).
