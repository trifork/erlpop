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

-spec parse(string()) -> {message, list({header, string(), string()}), string()}.
parse(Message) ->
    parse(Message, list).

-spec bin_parse(binary()) -> {message, list({header, binary(), binary()}), binary()}.
bin_parse(Message) ->
    parse(Message, binary).

-spec parse(string() | binary(), list | binary) -> {message, list({header, string() | binary(), string() | binary()}), string() | binary()}.
parse(Message, ReturnType) ->
    case re:split(Message, "\r\n\r\n", [{return, ReturnType}, {parts, 2}]) of
      [Header, Body] -> Headers = parse_headers(Header, ReturnType),
                        {message, Headers, Body};
      [_NotFound] -> error({parse_failed, end_of_header_not_found})
    end.

-spec parse_headers(string() | binary(), list | binary) -> list({header, string() | binary(), string() | binary()}).
parse_headers(Header, ReturnType) ->
    %% do "unfolding" first
    %% read more about unfolding long header fields here - http://www.faqs.org/rfcs/rfc2822.html
    %% "Unfolding is accomplished by simply removing any CRLF
    %% that is immediately followed by white space"
    UnfoldedHeader = re:replace(Header, "\r\n(?=[ \t])", "", [{return, ReturnType}, global]),
    RawHeaders = re:split(UnfoldedHeader, "\r\n", [{return, ReturnType}]),
    %io:format("raw headers = ~n~p~n", [RawHeaders]),
    ParseHeaderFun = fun(RawHeader) -> parse_header(RawHeader, ReturnType) end,
    lists:map(ParseHeaderFun, RawHeaders).

-spec parse_header(string() | binary(), list | binary) -> {header, string() | binary(), string() | binary()}.
parse_header(RawHeader, ReturnType) ->
    case re:split(RawHeader, "\s*:\s*", [{return, ReturnType}, {parts, 2}]) of
      [HeaderName, HeaderVal] -> {header,
                                  string:trim(HeaderName, leading),
                                  string:trim(HeaderVal, trailing)};
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
