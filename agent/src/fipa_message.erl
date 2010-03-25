-module(fipa_message).
-compile(export_all).

-include("fipa_message.hrl").
-include_lib("xmerl/include/xmerl.hrl").

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
		Message = #fipa_message{act = Act, sender = Sender, ontology = Ontology, content = Content},
		io:format("-> Message: ~p~n", [Message]);

	    false ->
		%%io:format("Unknown ontology: ~s~n", [Ontology#xmlAttribute.value])
		false
	end
    catch
	_:Reason ->
	    io:format("~nException:~p~n", [Reason]),
	    false
    end.
