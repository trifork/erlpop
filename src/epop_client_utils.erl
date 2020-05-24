-module(epop_client_utils).
-author('tobbe@serc.rmit.edu.au').

%%%---------------------------------------------------------------------
%%% File    : epop_client_utils.erl
%%% Created : 10 Sep 2008 by harish.mallipeddi@gmail.com
%%%           Origninal code was in epop.erl, see: 
%%%           https://github.com/gebi/jungerl/blob/master/lib/epop/src/epop.erl
%%% Function: Some helper utils for the client.
%%% ====================================================================
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%      http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% The Original Code is epop-2-3
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%---------------------------------------------------------------------

-export([recv_sl/1,recv_ml/1,recv_ml_on_ok/1,tokenize/1]).
-export([line_by_line_start/2,line_by_line_next/1,line_by_line_after/1,get_client_from_acc/1]).

-include("epop_client.hrl").

-define(CR, 13).
-define(LF, 10).


-ifndef(ssl_api).
-define(ssl_api, ssl).
-endif.

-ifndef(gen_tcp_api).
-define(gen_tcp_api, gen_tcp).
-endif.

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

%% accumulator type for chunked/stream retrieval of data
-type accumulator() :: {ok | halted, #sk{}, list(string()), any()}.

%% Create accumulator.
-spec line_by_line_start(#sk{}, any()) -> {ok, #sk{}, [], _}.
line_by_line_start(S, Data) ->
   {ok, S, [], Data}.  % return accumulator
    
%% return next line and new accumulator.
-spec line_by_line_next(accumulator()) -> {halt, accumulator()} | {string(), accumulator()}.
line_by_line_next(Acc) ->
    {State, S, T, Eol} = Acc,
    case State of 
      halted -> {halt, Acc};
      ok -> {NewLine, NewAcc} = rml(1, S, T, [], Eol, stream_lines),
            {NewLine ++ Eol, NewAcc}
    end.

%% End streaming. Read from socket until CRLF.CRLF termination.
-spec line_by_line_after(accumulator()) -> ok.
line_by_line_after(Acc) ->
    {State, S, T, Eol} = Acc,
    case State of 
      halted -> ok;
      ok -> try 
               rml(1,S,T,[],Eol,dev_null),
               ok
            catch 
               exit: {error,Reason} -> {error,Reason}
            end
    end.

-spec get_client_from_acc(accumulator()) -> #sk{}.
get_client_from_acc({_State, S, _Mline, _Data}) -> S.

recv_3_chars(S) -> recv_3_chars(S,recv(S)).

recv_3_chars(_,Cs) when length(Cs)>=3 -> Cs;
recv_3_chars(S,Cs) -> recv_3_chars(S,Cs ++ recv(S)).

%% ------------------------------------------
%% Receive a CRLF.CRLF terminated multi-line.
%% ------------------------------------------

recv_ml(S) ->
    recv_ml(S,[]).

recv_ml(S,Cc) ->
   {CharList, {halted, _S,T, [] }} = rml(1,S,Cc,[], [], char_list),
   {CharList,T}.

%% A simple state-event machine to handle the byte stuffing
%% of the termination octet. See also page.2 in the RFC-1939.
%% Since we are using a raw socket we are using this
%% continuation based style of programming.
%% 
%% State machine starts at state 1.
%% If a Carriage return + Line feed is followed by a dot, the dot will be removed, except when followed by another CR + LF.
%% A CR + LF + dot + CR + LF is the termination marker to end reading. In this case remove CR + LF + dot.
%% When type is stream_lines, return when a line is read (State 0). Return the line without the ending CR + LF.
%%
%% Schema: State Input New state
%% 0    -> 1 
%% 1 CR -> 2
%% 1    -> 1
%% 2 CR -> 2
%% 2 LF -> 3
%% 3 .  -> 4 remove dot
%% 3    -> 0
%% 4 CR -> 5
%% 4    -> 0 
%% 5 LF -> 6 remove CR LF
%% 5    -> 0
%% 6    -> ready
%% If not enough input data -> call recv and continue at the same State

%% accept line
rml(0, S,T,[?LF,?CR|Mline],Data,stream_lines) -> {lists:reverse(Mline),{ok,S, T, Data}};  % return new line without CR + LF
rml(0, S,T,_Mline,_Data,dev_null)           -> rml(1,S,T,[],[],dev_null);            % eat new line & continue
rml(0, S,T,Mline,Data,char_list)            -> rml(1,S,T,Mline,Data,char_list);      % add new line & continue

%% start here
rml(1,S,[?CR|T],Mline,Data,Type)            -> rml(2,S,T,[?CR|Mline],Data,Type);     % goto next state
rml(1,S,[H|T],Mline,Data,Type)              -> rml(1,S,T,[H|Mline],Data,Type);       % stay

%% CR accepted
rml(2,S,[?LF|T],Mline,Data,Type)            -> rml(3,S,T,[?LF|Mline],Data,Type);     % goto next state
rml(2,S,[?CR|T],Mline,Data,Type)            -> rml(2,S,T,[?CR|Mline],Data,Type);     % stay
rml(2,S,[H|T],Mline,Data,Type)              -> rml(1,S,[H|T],Mline,Data,Type);           % continue

%% CR + LF accepted
rml(3,S,[$.|T],Mline,Data,Type)             -> rml(4,S,T, Mline,Data,Type);          % remove dot, goto next state
rml(3,S,[H|T],Mline,Data,Type)              -> rml(0,S,[H|T], Mline,Data,Type);          % accept line & continue

%% CR + LF + . accepted
rml(4,S,[?CR|T],Mline,Data,Type)            -> rml(5,S,T,[?CR|Mline],Data,Type);     % goto next state
rml(4,S,[H|T],Mline,Data,Type)              -> rml(0,S,[H|T],Mline,Data,Type);           % accept line & continue

%% CR + LF + . + CR accepted
rml(5,S,[?LF|T],[?CR|Mline],Data,Type)      -> rml(6,S,T, Mline,Data,Type);          % remove CR + LF, goto next state
rml(5,S,[H|T],[?CR|Mline],Data,Type)        -> rml(0,S,[?CR,H|T],Mline,Data,Type);     % CR back in input, accept line & continue

%% CR + LF + . + CR + LF accepted, terminate reading
rml(6,S,T, [?LF,?CR|Mline],Data,stream_lines) -> {lists:reverse(Mline),{halted,S, T, Data}};  %% return last line without CR + LF
rml(6,S,T, _Mline,Data,dev_null) -> {[], {halted,S,T, Data }};
rml(6,S,T,  Mline,Data,char_list) -> {lists:reverse(Mline),{halted,S,T,Data} };

%% retrieve more data from socket
rml(State,S,[],Mline,Data,Type)   -> rml(State,S,recv(S),Mline,Data,Type).           % get more


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

complete_sl_lf(_,[?LF|T],Line) ->
    {lists:reverse([?LF|Line]),T};
complete_sl_lf(S,[_|T],Line) ->
    complete_sl(S,T,[?LF|Line]);
complete_sl_lf(S,[],Line) ->
    complete_sl_lf(S,recv(S),Line).

recv(S) ->
    case recv_proto(S) of
        {ok,Packet} -> Packet;
        Else        -> exit(Else)  %% for example Else = {error, timeout} or {error, closed}
    end.

recv_proto(S) ->
    case S#sk.ssl of
        true  ->     ?ssl_api:recv(S#sk.sockfd,0);
        false -> ?gen_tcp_api:recv(S#sk.sockfd,0)
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
