-module(epop_client_utils).
-author("Harish Mallipeddi <harish.mallipeddi@gmail.com>").

-export([recv_sl/1,recv_ml/1,recv_ml_on_ok/1,tokenize/1]).

-define(CR, 13).
-define(LF, 10).

%% ---------------------------------------------------------
%% If we are receiving a positive response, then receive
%% it as a multi-line response. Otherwise as a single-line.
%% ---------------------------------------------------------

recv_ml_on_ok(S) ->
    case recv_3_chars(S) of
	[$+,$O,$K|T] ->
	    recv_ml(S,[$+,$O,$K|T]);
	Else ->
	    recv_sl(S,Else)
    end.

recv_3_chars(S) -> recv_3_chars(S,recv(S)).

recv_3_chars(S,Cs) when length(Cs)>=3 -> Cs;
recv_3_chars(S,Cs) -> recv_3_chars(S,Cs ++ recv(S)).

%% ------------------------------------------
%% Receive a CRLF.CRLF terminated multi-line.
%% ------------------------------------------

recv_ml(S) ->
    recv_ml(S,[]).

recv_ml(S,Cc) ->
    rml(1,S,Cc,[]).

%% A simple state-event machine to handle the byte stuffing
%% of the termination octet. See also page.2 in the RFC-1939.
%% Since we are using a raw socket we are using this
%% continuation based style of programming.

rml(1,S,[?CR|T],Mline)        -> rml(2,S,T,[?CR|Mline]);     % goto next state
rml(1,S,[?LF|T],Mline)        -> rml(3,S,T,[?LF|Mline]);     % goto next state
rml(1,S,[H|T],Mline)          -> rml(1,S,T,[H|Mline]);       % stay

rml(2,S,[?LF|T],Mline)        -> rml(3,S,T,[?LF|Mline]);     % goto next state
rml(2,S,[H|T],Mline)          -> rml(1,S,[H|T],Mline);       % continue

rml(3,S,[$.|T],Mline)         -> rml(4,S,T,[$.|Mline]);      % goto next state
rml(3,S,[H|T],Mline)          -> rml(1,S,[H|T],Mline);       % continue

rml(4,S,[?CR|T],Mline)        -> rml(5,S,T,[?CR|Mline]);     % goto next state
rml(4,S,[?LF|T],Mline)        -> rml(6,S,T,[?LF|Mline]);     % goto next state
rml(4,S,[H|T],[$.|Mline])     -> rml(1,S,[H|T],Mline);       % continue

rml(5,S,[?LF|T],Mline)        -> rml(6,S,T,[?LF|Mline]);     % goto next state
rml(5,S,[H|T],[$.|Mline])     -> rml(1,S,[H|T],Mline);       % (de-)byte stuff

rml(6,S,T,[?LF,?CR,$.|Mline]) -> {lists:reverse(Mline),T};   % accept
rml(6,S,T,[?LF,$.|Mline])     -> {lists:reverse(Mline),T};   % accept

rml(State,S,[],Mline)         -> rml(State,S,recv(S),Mline). % get more


%% -----------------------------------------------------
%% Receive a complete single-line (ended by a CRLF pair.
%% Returns: {Single-Line, Continuation-Characters (Cc) }
%% Where Cc is the characters next to be processed.
%% -----------------------------------------------------

recv_sl(S) ->
    recv_sl(S,[]).

recv_sl(S,Cc) ->
    complete_sl(S,Cc,[]).

complete_sl(S,[?CR|T],Line) ->
    complete_sl_lf(S,T,[?CR|Line]);
complete_sl(S,[H|T],Line) ->
    complete_sl(S,T,[H|Line]);
complete_sl(S,[],Line) ->
    complete_sl(S,recv(S),Line).

complete_sl_lf(S,[?LF|T],Line) ->
    {lists:reverse([?LF|Line]),T};
complete_sl_lf(S,[H|T],Line) ->
    complete_sl(S,T,[?LF|Line]);
complete_sl_lf(S,[],Line) ->
    complete_sl_lf(S,recv(S),Line).

recv(S) ->
    case gen_tcp:recv(S,0) of
	{ok,Packet} -> Packet;
	Else        -> exit(Else)
    end.

%% -----------------------------------------------
%% Tokenize using \r\n as the two separators.
%% -----------------------------------------------

tokenize(L) ->
    tokenize(skip(L),[]).

tokenize([],Acc) ->
    lists:reverse(Acc);
tokenize(L,Acc) ->
    {Token,Rest} = get_token(L),
    tokenize(skip(Rest),[Token|Acc]).

skip([$\r,$\n|T]) -> skip(T);
skip(L)       -> L.

get_token(L) -> get_token(L,[]).

get_token([H|T],Acc) when H=/=$\r,H=/=$\n ->
    get_token(T,[H|Acc]);
get_token(L,Acc) -> {lists:reverse(Acc),L}.
