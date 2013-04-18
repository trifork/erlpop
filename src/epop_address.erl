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

-export([parse_list/1]).

-type name() :: string() | undefined.
-type address() :: string().

%%% Parse an address-list according to RFC5322.
-spec parse_list/1 :: (string()) -> [{address(), name()}].
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
address_list_p1([C | _S], _Acc) -> {error, {syntax_error_near, [C]}}.

%   address         =   mailbox / group
%   mailbox         =   name-addr / addr-spec
%   group           =   display-name ":" [group-list] ";" [CFWS]
address([]) -> throw({parse_error, "Expected address"});
address(S) -> address(skip_ws(S), []).

address([$< | S], NameAcc) ->
    {Addr, S2} = addr_spec(S),
    Name = lists:concat(lists:reverse(pop_ws(NameAcc))),
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
    LocalPart = lists:concat(lists:reverse(pop_ws(NameAcc))),
    {Address, S2} = addr_spec_p2(LocalPart, S),
    {{Address, ""}, S2};
address(X, NameAcc) ->
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
    {lists:concat(lists:reverse(Acc)), skip_ws(S)}.

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
    {lists:concat(lists:reverse(Acc)), S};
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
    {lists:concat(lists:reverse(Acc)), S};
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
