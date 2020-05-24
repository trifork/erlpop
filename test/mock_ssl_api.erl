-module(mock_ssl_api).
-export([recv/2]).


recv(SslSocket, _Length) ->
    Queue = erlang:get(SslSocket),
    {Result, NewQueue} = queue:out(Queue),
    erlang:put(SslSocket, NewQueue),
    case Result of
      empty -> {error, closed};
      {value, Packet} -> {ok, Packet}
    end.
