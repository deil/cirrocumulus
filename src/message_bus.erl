-module(message_bus).

-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").
-include_lib("exmpp_jid.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-include("jabber_config.hrl").
-include("fipa_message.hrl").

-compile(export_all).

%%
%% <fipa-message act="inform" ontology="/cirrocumulus-vu_manager"><content>reboot_vu 111</content></fipa-message>
%%

init(Cirrocumulus, Brain, Resource) ->
	logger:start(?MODULE),
	application:start(exmpp),
	MySession = exmpp_session:start(),
	Jid = get_hostname() ++ "-" ++ Resource,
	MyJID = exmpp_jid:make(Jid, ?Hostname, "Cirrocumulus"),
	logger:log(?MODULE, io_lib:format("logging as ~p", [MyJID])),
	%%io:format("MessageBus: logging as ~p~n", [MyJID]),
	exmpp_session:auth_basic_digest(MySession, MyJID, binary_to_list(MyJID#jid.node)),
	{ok, _StreamId} = exmpp_session:connect_TCP(MySession, ?Server, ?Port),
	session(MySession, MyJID),
	loop(MySession, MyJID, Cirrocumulus, Brain).

get_hostname() ->
	{ok, Hostname} = inet:gethostname(),
	Hostname.

session(MySession, _MyJID) ->
	%% Login with defined JID / Authentication:
	try
		exmpp_session:login(MySession)
	catch
		{auth_error, 'not-authorized'} ->
		  %% Try creating a new user:
			io:format("Register~n", []),
			%% In a real life client, we should trap error case here
			%% and print the correct message.
			exmpp_session:register_account(MySession, _MyJID#jid.node),
			%% After registration, retry to login:
			exmpp_session:login(MySession)
	end,
	exmpp_session:send_packet(MySession, exmpp_presence:set_status(exmpp_presence:available(), "Cirrocumulus")),
	join_groupchat(MySession, _MyJID).

join_groupchat(MySession, MyJID) ->
	Packet = exmpp_xml:set_attribute(#xmlel{name = 'presence'}, to, ?Chatroom ++ "/" ++ MyJID#jid.node),
	exmpp_session:send_packet(MySession, Packet).

loop(MySession, MyJID, Cirrocumulus, Brain) ->
	Self = self(),

	receive
		stop ->
			io:format("MessageBus: stop~n"),
			exmpp_session:stop(MySession);

		{message, Text} ->
			send_message(MySession, Text),
			loop(MySession, MyJID, Cirrocumulus, Brain);
			
		{Cirrocumulus, send_message, Act, Ontology, Receiver, InReplyTo, Reply} ->
			send_fipa_message(MySession, #fipa_message{act = Act, receiver = Receiver#agent_identifier.name,
																ontology = Ontology, in_reply_to = InReplyTo, content = Reply}),
			loop(MySession, MyJID, Cirrocumulus, Brain);

		Record = #received_packet{packet_type=message, raw_packet=Packet} ->
			try
				%%io:format("--> ~s~n", [exmpp_xml:document_to_binary(Packet)]),
				From = exmpp_xml:get_attribute(Packet, from, undefined),
				HasBody = exmpp_xml:has_element(Packet, body),
				MyMessage = message_from_self(From),
				ShouldProcess = HasBody and not MyMessage and not is_old_message(Packet),
				case ShouldProcess of
	    		true ->
						BodyElem = exmpp_xml:get_element(Packet, body),
						Body = exmpp_xml:get_cdata(BodyElem),
						Message = fipa_message:parse_message(Body, Brain),
						if
							Message /= false ->
								logger:log(?MODULE, io_lib:format("got message -> ~p", [Message])),
								Cirrocumulus ! {Self, receive_message, Message};
							
							true ->
								false
						end;
					
					_ -> false
				end
			catch
				_:Reason ->
					false
			end,
			loop(MySession, MyJID, Cirrocumulus, Brain);
            
		Record ->
			%%io:format("~p~n", [Record]),
			loop(MySession, MyJID, Cirrocumulus, Brain)
	end.

is_old_message(Packet) ->
	exmpp_xml:has_element(Packet, x).

message_from_self(From) ->
	Res = regexp:match(binary_to_list(From), get_hostname()),
	case Res of
		nomatch -> false;
		_ -> true
	end.

send_fipa_message(MySession, Message=#fipa_message{}) ->
		EmptyMsg = #xmlel{name = "fipa-message"},
		MsgWithAct = exmpp_xml:set_attribute(EmptyMsg, act, Message#fipa_message.act),
		MsgWithOntology = exmpp_xml:set_attribute(MsgWithAct, ontology, Message#fipa_message.ontology),
		send_message(MySession, exmpp_xml:document_to_binary(MsgWithOntology)).

send_message(MySession, Text) ->
    Msg = #xmlel{name = "message"},
    Msg1 = exmpp_xml:set_attribute(Msg, type, <<"groupchat">>),
    Msg2 = exmpp_xml:set_attribute(Msg1, to, ?Chatroom),
    Body = #xmlel{name = "body"},
    Body1 = exmpp_xml:set_cdata(Body, Text),
    Packet = exmpp_xml:append_child(Msg2, Body1),
    exmpp_session:send_packet(MySession, Packet).

send_message(MySession, To, Text) ->
    Msg = #xmlel{name = "message"},
    Msg1 = exmpp_xml:set_attribute(Msg, type, <<"chat">>),
    Msg2 = exmpp_xml:set_attribute(Msg1, to, To),
    Body = #xmlel{name = "body"},
    Body1 = exmpp_xml:set_cdata(Body, Text),
    Packet = exmpp_xml:append_child(Msg2, Body1),
    exmpp_session:send_packet(MySession, Packet).

process_message(MySession, MyJID, From, Text) ->
    El1 = #xmlel{name = 'fipa-message'},
    El2 = exmpp_xml:set_attribute(El1, act, "inform"),
    Sender1 = #xmlel{name = 'sender'},
    Sender = exmpp_xml:set_attribute(Sender1, name, MyJID#jid.node),
    Ontology1 = #xmlel{name = 'ontology'},
    Ontology = exmpp_xml:set_cdata(Ontology1, "cirrocumulus"),
    El3 = exmpp_xml:append_child(El2, Sender),
    El4 = exmpp_xml:append_child(El3, Ontology),
    Content1 = #xmlel{name = 'content'},
    Content = exmpp_xml:set_cdata(Content1, binary_to_list(Text)),
    El = exmpp_xml:append_child(El4, Content),
    send_message(MySession, exmpp_xml:document_to_binary(El)).

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
