-module(epop_message).
-author("Harish Mallipeddi <harish.mallipeddi@gmail.com>").

%%%---------------------------------------------------------------------
%%% File    : epop_message.erl
%%% Created : 17 Sep 2008 by harish.mallipeddi@gmail.com
%%% Function: Utility to parse email messages
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
%%%---------------------------------------------------------------------

-export([parse/1, find_header/2, bin_parse/1]).

-spec parse(string()) -> {message, list({header,string(),string()}), string()}.
parse(Message) ->
    Boundary = string:str(Message, "\r\n\r\n"),
    Boundary>0 orelse error({parse_failed, end_of_header_not_found}),
    Header = string:substr(Message, 1, Boundary - 1),
    Body = string:substr(Message, Boundary + 4),
    Headers = parse_headers(Header),
    {message, Headers, Body}.

-spec bin_parse(binary()) -> {message, list({header, binary(), binary()}), binary()}.
bin_parse(Message) ->
    case re:split(Message, <<"\r\n\r\n">>, [{parts, 2}]) of
      [Header, Body] -> Headers = bin_parse_headers(Header),
                        {message, Headers, Body}; 
      [_NotFound] -> error({parse_failed, end_of_header_not_found})
    end.

-spec parse_headers(string()) -> list({header,string(),string()}).
parse_headers(Header) ->
    %% do "unfolding" first
    %% read more about unfolding long header fields here - http://www.faqs.org/rfcs/rfc2822.html
    %% "Unfolding is accomplished by simply removing any CRLF
    %% that is immediately followed by white space"
    UnfoldedHeader = re:replace(Header, "\r\n(?=[ \t])","", [{return,list},global]),
    RawHeaders = re:split(UnfoldedHeader, "\r\n", [{return, list}]),
    %io:format("raw headers = ~n~p~n", [RawHeaders]),
    lists:map(fun parse_header/1, RawHeaders).

-spec bin_parse_headers(binary()) -> list({header, binary(), binary()}).
bin_parse_headers(Header) ->
    %% do "unfolding" first
    %% read more about unfolding long header fields here - http://www.faqs.org/rfcs/rfc2822.html
    %% "Unfolding is accomplished by simply removing any CRLF
    %% that is immediately followed by WSP"
    UnfoldedHeader = re:replace(Header, "\r\n(?=[ \t])","", [{return,binary},global]),
    RawHeaders = re:split(UnfoldedHeader, "\r\n", [{return,binary}]),
    %io:format("raw headers = ~n~p~n", [RawHeaders]),
    lists:map(fun bin_parse_header/1, RawHeaders).

-spec parse_header(string()) -> {header,string(),string()}.
parse_header(RawHeader) ->
    Boundary = string:str(RawHeader, ":"),
    Boundary>0 orelse error({parse_failed, end_of_header_name_found}),
    HeaderName = string:trim(string:substr(RawHeader, 1, Boundary - 1)),
    HeaderVal = string:trim(string:substr(RawHeader, Boundary + 1)),
    {header, HeaderName, HeaderVal}.

-spec bin_parse_header(binary()) -> {header, binary(), binary()}.
bin_parse_header(RawHeader) ->
    case re:split(RawHeader, <<"\s*:\s*">>, [{parts, 2}]) of
      [HeaderName, HeaderVal] -> {header, 
                                  re:replace(HeaderName, "^\\s+", "",[{return,binary}]),
                                  re:replace(HeaderVal,  "\\s+$", "",[{return,binary}])};
      [_NotFound] -> error({parse_failed, end_of_header_name_found})
    end.

%% Find the first header given the name from the headers list.
%%
%% Notice that some headers like 'Received' and 'Return-Path' can occur multiple times,
%% but this function will only return the first header.
-spec find_header(list({header, string() | binary(), string() | binary()}), string() | binary()) -> {ok, string() | binary()} | {error, not_found}.
find_header(HeaderList, HeaderName) ->
    LcHeaderName = string:lowercase(HeaderName),
    ContinueSearchFun = fun({header,Key,_Value}) -> 
                            LcHeaderName =/= string:lowercase(Key)
                        end, 
    case lists:dropwhile(ContinueSearchFun, HeaderList) of
      [] -> {error, not_found};
      [{header,_Key,Value}| _] -> {ok, Value}
    end.
