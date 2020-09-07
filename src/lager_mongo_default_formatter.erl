-module(lager_mongo_default_formatter).

-export([format/1, format/2]).

-spec format(Message::lager_msg:lager_msg(), any()) -> bson:document().
format(Message, _Config) -> format(Message).

-spec format(Message::lager_msg:lager_msg()) -> bson:document().
format(Message) ->
    {M, S, _} = lager_msg:timestamp(Message),
    Metadata = lager_msg:metadata(Message),
    {<<"level">>, lager_msg:severity(Message),
     <<"time">>, M * 1000000 + S,
     <<"node">>, node(),
     <<"msg">>, to_unicode(lager_msg:message(Message)),
     <<"_module">>, to_binary(proplists:get_value(module, Metadata)),
     <<"_line">>, proplists:get_value(line, Metadata, 0)}.

to_binary(B) when is_binary(B) -> B;
to_binary(S) when is_list(S) -> list_to_binary(S);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(_) -> <<>>.

to_unicode(B) when is_binary(B) -> B;
to_unicode(S) when is_list(S) -> unicode:characters_to_binary(S);
to_unicode(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_unicode(_) -> <<>>.
