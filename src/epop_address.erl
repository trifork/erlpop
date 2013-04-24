-module(epop_address).
-author("Erik Søe Sørensen <eriksoe@gmail.com>").

%%%---------------------------------------------------------------------
%%% File    : epop_address.erl
%%% Function: Utility to parse email addresses and address lists
%%% ====================================================================
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.1, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.erlang.org/EPLICENSE
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%---------------------------------------------------------------------

%%% Purpose: Email address list parser.
%%% Status:
%%% Is complete enough to be better than nothing (and, presumably,
%%% better than simpler tools).
%%% Is not complete enough to be RFC compliant.
%%% It might be a good idea to use something like 'abfnc' for generating
%%% a compliant parser from the spec.

-export([parse_list/1, expand_groups/1]).

-type name() :: string() | undefined.
-type address() :: string().
-type named_address() :: {address(), name()}.
-type address_group() :: {group, GroupName::string(), [named_address()]}.

%%% Flatten address list by expanding groups.
-spec expand_groups/1 :: ([named_address() | address_group()]) -> [named_address()].
expand_groups(Items) when is_list(Items) ->
    lists:flatmap(fun({group, _GroupName, L}) -> L;
                     ({_,_}=Addr)             -> [Addr]
                  end,
                  Items).

%%% Parse an address-list according to RFC5322.
-spec parse_list/1 :: (string()) -> [named_address() | address_group()].
parse_list(S) ->
    try {ok, address_list(tokenize(S), [])}
    catch
        throw:{parse_error, Err} ->
            {error, Err}
    end.

address_list([], []) -> {error, empty_address_list};
address_list(S, Acc) ->
    {Item,S2} = address(S),
    address_list_p1(skip_ws(S2), [Item|Acc]).

address_list_p1([], Acc) -> lists:reverse(Acc);
address_list_p1([$, | S], Acc) -> address_list(S, Acc);
address_list_p1([C | _S], _Acc) ->
    throw({parse_error, "Syntax error near '"++to_str(C)++"'"}).

%% "Empty list allowed" version - and stop at ";":
address_list_in_group([$; | S]) ->
    % Empty group.
    {group_end, [], S};
address_list_in_group(S) ->
    address_list_in_group(S, []).

address_list_in_group([], Acc) ->
    % Missing group terminator. Allow for leniency.
    {group_end, lists:reverse(Acc), []};
address_list_in_group(S, Acc) ->
    {Item,S2} = address(skip_ws(S)),
    case skip_ws(S2) of
        [$, | S3] ->
            address_list_in_group(S3, [Item|Acc]);
        [$; | S3] ->
            {group_end, lists:reverse([Item|Acc]), S3};
        [C | _S3] ->
            throw({parse_error, "Syntax error near '"++to_str(C)++"'"})
    end.

%   address         =   mailbox / group
%   mailbox         =   name-addr / addr-spec
%   group           =   display-name ":" [group-list] ";" [CFWS]
address([]) -> throw({parse_error, "Expected address"});
address(S) -> address(skip_ws(S), []).

address([$< | S], NameAcc) ->
    {Addr, S2} = addr_spec(S),
    Name = acc_to_str(pop_ws(NameAcc)),
    case S2 of
        [$> | S3] -> {{Addr, Name},S3};
        [X | _] -> throw({parse_error, "Expected '>' but found '"++to_str(X)++"'"})
    end;
address([$" | _]=S, NameAcc) ->
    {W,S2} = quoted_string(S),
    address(S2, [W|NameAcc]);
address([L | S], NameAcc) when is_list(L) ->
    address(S, [L|NameAcc]);
address([C | S], NameAcc) when is_integer(C),
                               C =:= $.;
                               C =:= $\s;
                               C =:= $\t ->
    address(S, [[C]|NameAcc]);
address([$@ | S], NameAcc) ->
    %% Free-standing address without a name
    %% TODO: This is currently too lenient.
    LocalPart = acc_to_str(pop_ws(NameAcc)),
    {Address, S2} = addr_spec_p2(LocalPart, S),
    {{Address, ""}, S2};
address([$: | S], NameAcc) ->
    %% Named group
    %% TODO: Handle groups better
    GroupName = acc_to_str(pop_ws(NameAcc)),
    case address_list_in_group(skip_ws(S)) of
        {group_end, Addrs, S2} -> ok;
        Addrs -> S2="", ok % ";" was missing
    end,
    {{group, GroupName, Addrs}, S2};
address([$; | S], []) ->
    %% End of group.
    %% TODO: Handle case NameAcc /= []
    {none, S};
address([X | _]=_S, _NameAcc) ->
    throw({parse_error, "Invalid token in address: '"++to_str(X)++"'"}).

%   addr-spec       =   local-part "@" domain
addr_spec(S) ->
    {LocalPart,S2} = local_part(S),
    S3 = expect_special($@, S2),
    addr_spec_p2(LocalPart,S3).

addr_spec_p2(LocalPart,S3) ->
    {Domain,S4} = domain(S3),
    {{LocalPart,Domain}, S4}.

%   local-part      =   dot-atom / quoted-string / obs-local-part
local_part([$" | _]=S) -> quoted_string(S);
local_part(S) -> dot_atom(S).

%   dot-atom-text   =   1*atext *("." 1*atext)
%
%   dot-atom        =   [CFWS] dot-atom-text [CFWS]
dot_atom(S) -> dot_atom(skip_ws(S), []).

dot_atom([$. | S], Acc) ->
    dot_atom(S, [[$.] | Acc]);
dot_atom([L | S], Acc) when is_list(L) ->
    dot_atom(S, [L | Acc]);
dot_atom(S, Acc) ->
    {acc_to_str(Acc), skip_ws(S)}.

%   domain          =   dot-atom / domain-literal / obs-domain
domain(S) -> domain_after_ws(skip_ws(S)).

domain_after_ws([$[ | _]=S) ->
    {DL,S2} = domain_literal(S),
    {{literal, DL}, S2};
domain_after_ws(S) -> dot_atom(S).

%   domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
%   dtext           =   %d33-90 /          ; Printable US-ASCII
%                       %d94-126 /         ;  characters not including
%                       obs-dtext          ;  "[", "]", or "\"
domain_literal([$[ | S]) -> domain_literal(skip_ws(S), []).

domain_literal([$] | S], Acc) ->
    {acc_to_str(Acc), S};
domain_literal([C | _S], _Acc) when C =:= $[;
                                    C =:= $\\ ->
    throw({parse_error, "Invalid character in domain-literal: '"++[C]++"'"});
domain_literal([C | S], Acc) when is_integer(C) ->
    domain_literal(S, [[C]|Acc]);
domain_literal([L | S], Acc) when is_list(L) ->
    domain_literal(S, [L|Acc]).

quoted_string([$" | S]) -> %"]) ->
    quoted_string_aux(S, []).

quoted_string_aux([], _Acc) ->
    throw({parse_error, "Unterminated quoted string"});
quoted_string_aux([$" | S], Acc) -> %"]) ->
    %% End of string.
    {acc_to_str(Acc), S};
quoted_string_aux([$\\, C | S], Acc) when C =:= $\t;
                                          C >= $\s andalso C =< 126 ->
    %% quoted-pair.
    quoted_string_aux(S, [[C] | Acc]);
quoted_string_aux([$\\, [C | S1]| S2], Acc) when C >= $\s andalso C =< 126 ->
    %% quoted-pair.
    quoted_string_aux([S1|S2], [[C] | Acc]);
quoted_string_aux([L | S], Acc) when is_list(L) ->
    quoted_string_aux(S, [L|Acc]);
quoted_string_aux([C | S], Acc) when is_integer(C) ->
    quoted_string_aux(S, [[C]|Acc]).

%%%========== Helper functions ========================================

to_str(C) when is_integer(C) -> [C];
to_str(L) when is_list(L) -> L.

acc_to_str(L) ->
    lists:concat(lists:reverse(L)).

%%% Tokenize according to 'specials'.
tokenize([]) -> [];
tokenize(S=[H|T]) ->
    case string:cspan(S, "()<>[]:;@\\,.\" \t") of
        0 ->
            [H | tokenize(T)];
        N ->
            {W,S2} = lists:split(N, S),
            [W | tokenize(S2)]
    end.

expect_special(C, [C|S]) -> S;
expect_special(C, [Other|_S]) ->
    {parse_error, "Expected '"++[C]++"' but found '"++to_str(Other)++"'"};
expect_special(C, []) ->
    {parse_error, "Expected '"++[C]++"' but found end-of-string"}.

pop_ws([" " | S]) -> pop_ws(S);
pop_ws(["\t" | S]) -> pop_ws(S);
pop_ws(S) -> S.

skip_ws([$\s | S]) -> skip_ws(S);
skip_ws([$\t | S]) -> skip_ws(S);
skip_ws(S) -> S.
