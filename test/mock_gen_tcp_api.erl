-module(mock_gen_tcp_api).
-export([recv/2]).


recv(_SslSocket, _Length) ->
    Packet = "Date: Sat, 12 Aug 2017 09:26:56 +0000\r\n\r\nBody\r\n.\r\nAfter terminating marker\r\n", 
    {ok, Packet}.
