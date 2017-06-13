-module(epop_client_utils).
-author("Harish Mallipeddi <harish.mallipeddi@gmail.com>").

%%%---------------------------------------------------------------------
%%% File    : epop_client_utils.erl
%%% Created : 10 Sep 2008 by harish.mallipeddi@gmail.com
%%% Function: Some helper utils for the client.
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
%%% Contents of this file are taken from the original epop code.
%%%---------------------------------------------------------------------

-export([recv_sl/1,recv_ml/1,recv_ml_on_ok/1,tokenize/1]).
-export([bin_recv_ml_on_ok/1]).

-define(CR, 13).
-define(LF, 10).
-define(CHUNK_SIZE,33000).

-include("epop_client.hrl").

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

bin_recv_ml_on_ok(S) ->
    case recv_3_chars(S) of
	    [$+,$O,$K|T] ->
	        bin_recv_ml(S,[$+,$O,$K|T]);
	    Else ->
	        bin_recv_sl(S,Else)
    end.

recv_3_chars(S) -> recv_3_chars(S,recv(S)).

recv_3_chars(_,Cs) when length(Cs)>=3 -> Cs;
recv_3_chars(S,Cs) -> recv_3_chars(S,Cs ++ recv(S)).

%% ------------------------------------------
%% Receive a CRLF.CRLF terminated multi-line.
%% ------------------------------------------

recv_ml(S) ->
    recv_ml(S,[]).

recv_ml(S,Cc) ->
    rml(1,S,Cc,[],?CHUNK_SIZE, []).

bin_recv_ml(S,Cc) ->
    rml(1,S,Cc,[],?CHUNK_SIZE, << >>).

%% A simple state-event machine to handle the byte stuffing
%% of the termination octet. See also page.2 in the RFC-1939.
%% Since we are using a raw socket we are using this
%% continuation based style of programming.
%% 
bin_append_reverse_list(Bin,Mline) when is_binary(Bin) -> 
  BinAdd = erlang:list_to_binary(lists:reverse(Mline)),
  << Bin/binary, BinAdd/binary >>;

bin_append_reverse_list(NoBin,_Mline) -> NoBin.

empty_list_if_append(Bin,_Mline) when is_binary(Bin) -> [];
empty_list_if_append(_,Mline) -> Mline.

rml_ready(Bin,Mline) when is_binary(Bin) -> 
  bin_append_reverse_list(Bin,Mline);
rml_ready(_NoBin,Mline) -> 
  lists:reverse(Mline).

rml(S1,S2,T,Mline,0,Bin)            -> rml(S1,S2,T,empty_list_if_append(Bin,Mline),?CHUNK_SIZE,bin_append_reverse_list(Bin,Mline));

rml(1,S,[?CR|T],Mline,C,Bin)        -> rml(2,S,T,[?CR|Mline],C-1,Bin);     % goto next state
rml(1,S,[?LF|T],Mline,C,Bin)        -> rml(3,S,T,[?LF|Mline],C-1,Bin);     % goto next state
rml(1,S,[H|T],Mline,C,Bin)          -> rml(1,S,T,[H|Mline],C-1,Bin);       % stay

rml(2,S,[?LF|T],Mline,C,Bin)        -> rml(3,S,T,[?LF|Mline],C-1,Bin);     % goto next state
rml(2,S,[H|T],Mline,C,Bin)          -> rml(1,S,[H|T],Mline,C,Bin);         % continue

rml(3,S,[$.|T],Mline,C,Bin)         -> rml(4,S,T,[$.|Mline],C-1,Bin);      % goto next state
rml(3,S,[H|T],Mline,C,Bin)          -> rml(1,S,[H|T],Mline,C,Bin);         % continue

rml(4,S,[?CR|T],Mline,C,Bin)        -> rml(5,S,T,[?CR|Mline],C-1,Bin);     % goto next state
rml(4,S,[?LF|T],Mline,C,Bin)        -> rml(6,S,T,[?LF|Mline],C-1,Bin);     % goto next state
rml(4,S,[H|T],[$.|Mline],C,Bin)     -> rml(1,S,[H|T],Mline,C,Bin);         % continue

rml(5,S,[?LF|T],Mline,C,Bin)        -> rml(6,S,T,[?LF|Mline],C-1,Bin);     % goto next state
rml(5,S,[H|T],[$.|Mline],C,Bin)     -> rml(1,S,[H|T],Mline,C-1,Bin);       % (de-)byte stuff

rml(6,_,T,[?LF,?CR,$.|Mline],_C,Bin) -> {rml_ready(Bin,Mline),T};          % accept
rml(6,_,T,[?LF,$.|Mline],_C,Bin)     -> {rml_ready(Bin,Mline),T};          % accept

rml(State,S,[],Mline,C,Bin)         -> rml(State,S,recv(S),Mline,C,Bin).   % get more


%% -----------------------------------------------------
%% Receive a complete single-line (ended by a CRLF pair.
%% Returns: {Single-Line, Continuation-Characters (Cc) }
%% Where Cc is the characters next to be processed.
%% -----------------------------------------------------

recv_sl(S) ->
    recv_sl(S,[]).

recv_sl(S,Cc) ->
    complete_sl(S,Cc,[]).

bin_recv_sl(S,CC) ->
    {L,T} = recv_sl(S,CC),
    {erlang:list_to_binary(L),T}.

complete_sl(S,[?CR|T],Line) ->
    complete_sl_lf(S,T,[?CR|Line]);
complete_sl(S,[H|T],Line) ->
    complete_sl(S,T,[H|Line]);
complete_sl(S,[],Line) ->
    complete_sl(S,recv(S),Line).

complete_sl_lf(_,[?LF|T],Line) ->
    {lists:reverse([?LF|Line]),T};
complete_sl_lf(S,[_|T],Line) ->
    complete_sl(S,T,[?LF|Line]);
complete_sl_lf(S,[],Line) ->
    complete_sl_lf(S,recv(S),Line).

recv(S) ->
    case recv_proto(S) of
        {ok,Packet} -> Packet;
        Else        -> exit(Else)
    end.

recv_proto(S) ->
    case S#sk.ssl of
        true -> ssl:recv(S#sk.sockfd,0);
        false -> gen_tcp:recv(S#sk.sockfd,0)
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
