-module(client_utils_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("epop_client.hrl").
epop_client_utils_recv_sl_test() ->
    % uses mock_gen_tcp
    Sender = #sk{},
    ?assertEqual({"Date: Sat, 12 Aug 2017 09:26:56 +0000\r\n", "\r\nBody\r\n.\r\nAfter terminating marker\r\n"}, 
                 epop_client_utils:recv_sl(Sender)).

set_title([H|_T]) ->
    lists:sublist(H, 14).

set_input(InputList) ->
    set_input(InputList, queue:new()).

set_input([], Queue) ->
    % mock_ssl gets its test data from the queue in the process dictionary.
    erlang:put(self(), Queue);
set_input([H|T], Queue) ->
    set_input(T, queue:in(H, Queue)).

recv_ml(Sender, Input) ->
    set_input(Input),
    epop_client_utils:recv_ml(Sender).

epop_client_utils_recv_ml_case_test_() ->
    Sender = #sk{ssl = true, sockfd = self()},
    % first recv call will return "test message \r", second recv call the "\n.\r\n". The \r\n.\r\n finishes multi-line read.
    set_input(["test message\r", "\n.\r\n", "this will not be read\r\n"]),
    ?assertEqual({"test message\r\n", []},
                 epop_client_utils:recv_ml(Sender)),
    %% Spec format: [{Input, ExpectedOutput}]
    Spec = [
            %% Basic test
              {["test 1 message", "\r\n.\r\n", "after"], {"test 1 message\r\n", []}}
            , {["test 2 message\r\n", ".\r\n", "after"], {"test 2 message\r\n", []}}
            , {["test 3 message\r\n.\r", "\n", "after"], {"test 3 message\r\n", []}}
            , {["test 4 message\r\n.\r\n", "after"],     {"test 4 message\r\n", []}}
            , {["test 5 message\r\n.dot removed\r\n.\r\n"],      {"test 5 message\r\ndot removed\r\n", []}}
            , {["test 6 message\r.dot not removed\r\r\n.\r\n"],  {"test 6 message\r.dot not removed\r\r\n", []}}
            , {["test 7 message\n.dot not removed\n\r\n.\r\n"],  {"test 7 message\n.dot not removed\n\r\n", []}}
            , {["test 8 message\r\n..one dot removed\n\r\n.\r\n"], {"test 8 message\r\n.one dot removed\n\r\n", []}}
            , {["test 9 message\r\n\r\n..one dot removed\n\r\n.\r\n"], {"test 9 message\r\n\r\n.one dot removed\n\r\n", []}}
            , {["test10 message\r\r\n.dot removed\r\n.\r\n"],          {"test10 message\r\r\ndot removed\r\n", []}}
            , {["test11 message\r\r\r\n.dot removed\r\r\r\n.\r\n"],    {"test11 message\r\r\r\ndot removed\r\r\r\n", []}}
            , {["test12 message\r\r\n", ".\r\n", "after"],             {"test12 message\r\r\n", []}}
           ],
    [{set_title(Input), % test title
      ?_assertEqual(Expected, recv_ml(Sender, Input))}
     || {Input, Expected} <- Spec].
