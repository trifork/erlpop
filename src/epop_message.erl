-module(epop_message).
-author("Harish Mallipeddi <harish.mallipeddi@gmail.com>").

%%%---------------------------------------------------------------------
%%% File    : epop_message.erl
%%% Created : 17 Sep 2008 by harish.mallipeddi@gmail.com
%%% Function: Utility to parse email messages
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

-export([parse/1, find_header/2]).

parse(Message) ->
    Boundary = string:str(Message, "\r\n\r\n"),
    Boundary>0 orelse error({parse_failed, end_of_header_not_found}),
    Header = string:substr(Message, 1, Boundary - 1),
    Body = string:substr(Message, Boundary + 4),
    Headers = parse_headers(Header),
    {message, Headers, Body}.

parse_headers(Header) ->
    %% do "unfolding" first
    %% read more about unfolding long header fields here - http://www.faqs.org/rfcs/rfc2822.html
    %% "Unfolding is accomplished by simply removing any CRLF
    %% that is immediately followed by WSP"
    UnfoldedHeader = re:replace(Header, "\r\n(?=[ \t])","", [{return,list},global]),
    RawHeaders = re:split(UnfoldedHeader, "\r\n", [{return, list}]),
    %io:format("raw headers = ~n~p~n", [RawHeaders]),
    lists:map(fun parse_header/1, RawHeaders).

parse_header(RawHeader) ->
    Boundary = string:str(RawHeader, ":"),
    Boundary>0 orelse error({parse_failed, end_of_header_name_found}),
    HeaderName = string:strip(string:substr(RawHeader, 1, Boundary - 1)),
    HeaderVal = string:strip(string:substr(RawHeader, Boundary + 1)),
    {header, HeaderName, HeaderVal}.

%% find the header given the name from the headers list
find_header([], _) ->
    {error, not_found};
find_header([{header, HeaderName, HeaderValue}|_Rest], Key) when HeaderName =:= Key ->
    {ok, HeaderValue};
find_header([_|Rest], Key) ->
    find_header(Rest, Key).
