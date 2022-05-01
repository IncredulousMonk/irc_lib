-module(irc_parser).

-export([
    split_nick/1,
    parse_irc/1
]).

-export_type([parsed_message/0]).

-type parsed_message() :: {Source::string(), Verb::string(), Params::[string()]}.

%% Utility function to split a nick (nick!ident@host)
split_nick(FullNick) ->
    [_FromNick | _IdentHost] = re:split(FullNick, "!", [{parts, 2}, {return, list}]).

%% https://github.com/npwolf/erlbot/blob/master/src/irc_router.erl
%% Parse irc messages like Python's Twisted http://stackoverflow.com/questions/930700/python-parsing-irc-messages
%% parse_irc(<<":test!~test@test.com PRIVMSG #channel :Hey there">>)
%% {"test!~test@test.com", "PRIVMSG", ["#channel", "Hey there"]}
-spec parse_irc(string() | binary()) -> parsed_message().
parse_irc(Data) when is_list(Data) ->
    parse_irc(list_to_binary(Data));
parse_irc(<< ":", Rest/binary >>) ->
    [Prefix, NewRest] = re:split(Rest, " ", [{parts, 2}, {return, binary}]),
    parse_irc(Prefix, NewRest);
parse_irc(Rest) ->
    parse_irc(<<>>, Rest).

parse_irc(Prefix, Rest) ->
    RestStripped = binary:replace(Rest, <<"\r\n">>, <<>>, [global]),
    case re:run(RestStripped, " :") of 
        {match, _} ->
            [NewRest, Trailing] = re:split(RestStripped, " :", [{parts, 2}, {return, binary}]),
            ArgParts = re:split(NewRest, "\s+", [{return, binary}]),
            Args = [ binary_to_list(A) || A <- lists:append(ArgParts, [Trailing]) ];
        _ ->
            Temp = [ binary_to_list(A) || A <- re:split(RestStripped, "\s+", [{return, binary}]) ],
            Args = remove_trailing_empty_string(Temp)
    end,
    [Command | FinalArgs] = Args,
    PrefixString = binary_to_list(Prefix),
    {PrefixString, Command, FinalArgs}.

remove_trailing_empty_string(L) ->
    Rev = lists:reverse(L),
    Stripped = remove_empty_head(Rev),
    lists:reverse(Stripped).

remove_empty_head(["" | L]) -> L;
remove_empty_head(L) -> L.
