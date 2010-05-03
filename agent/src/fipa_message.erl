-module(fipa_message).
-compile(export_all).

-include("fipa_message.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%
%% <fipa-message act="inform" ontology="/cirrocumulus-account_monitor"><content>q</content></fipa-message>
%%

is_known_ontology(OntologyAttr, Brain) ->
	Brain ! {self(), get_ontology},
  receive
	{get_ontology, SupportedOntology} ->
		Pattern = "/" ++ SupportedOntology ++ "$",
		case regexp:match(OntologyAttr#xmlAttribute.value, Pattern) of
			nomatch -> false;
			_ -> true
		end
	end.

extract_ontology(OntologyAttr) ->
	{match, Start, Length} = regexp:match(OntologyAttr#xmlAttribute.value, "/cirrocumulus-"),
	string:substr(OntologyAttr#xmlAttribute.value, Start+1).

parse_sender(Xml) ->
    Senders = xmerl_xs:select("/fipa-message/sender", Xml),
    if
	length(Senders) == 1 ->
	    [Sender| _] = Senders,
	    [NameAttr] = xmerl_xs:select("/sender/@name", Sender),
	    #agent_identifier{name = NameAttr#xmlAttribute.value};

	length(Senders) == 0 -> undefined
    end.

parse_receiver(Xml) ->
	Receivers = xmerl_xs:select("/fipa-message/receiver", Xml),
	if
		length(Receivers) == 1 ->
			[Receiver|_] = Receivers,
			[NameAttr] = xmerl_xs:select("/receiver/@name", Receiver),
			#agent_identifier{name = NameAttr#xmlAttribute.value};
	
		length(Receivers) == 0 -> undefined
	end.

message_for(Message, Agent) ->
	if
		Message#fipa_message.receiver == Agent -> true;
		Message#fipa_message.receiver /= Agent -> false
	end.

parse_message(Binary, Brain) ->
	try
		String = binary_to_list(Binary),
		{Document, _} = xmerl_scan:string(String),
		[OntologyAttr] = xmerl_xs:select("/fipa-message/@ontology", Document),
		case fipa_message:is_known_ontology(OntologyAttr, Brain) of
	    true ->
				Ontology = fipa_message:extract_ontology(OntologyAttr),
				[ActAttr] = xmerl_xs:select("/fipa-message/@act", Document),
				Act = ActAttr#xmlAttribute.value,
				[ContentElem] = xmerl_xs:select("/fipa-message/content", Document),
				[Content] = xmerl_xs:value_of(ContentElem),
				Sender = fipa_message:parse_sender(Document),
				Receiver = fipa_message:parse_receiver(Document),
				Message = #fipa_message{act = Act, sender = Sender, receiver = Receiver, ontology = Ontology, content = Content};

	    false ->
				%%io:format("Unknown ontology: ~s~n", [OntologyAttr#xmlAttribute.value]),
				false
		end
  catch
		_:Reason ->
	   	io:format("~nException:~p~n", [Reason]),
	   	false
	end.
