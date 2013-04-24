-module(parser_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

message_parse_test() ->
    %% Tests:
    %% Headers are parsed.
    %% Continuation lines in headers are handled.
    %% CRLF-CRLFs in messages are kept.
    Message = "First-header: value\r\nTo: xyz\r\n sp\r\n\ttab\r\nFrom: from\r\n\r\nBody text\r\nLine 2\r\n\r\nLine 4",
    ?assertEqual({message,
                  [{header, "First-header", "value"},
                   {header, "To", "xyz sp\ttab"},
                   {header, "From", "from"}],
                  "Body text\r\nLine 2\r\n\r\nLine 4"},
                 epop_message:parse(Message)).

address_parse_happy_case_test_() ->
    %% Spec format: [{Input, ExpectedOutput}]
    Spec = [
            %% Basic test
            {"<a@b.com>", [{{"a","b.com"}, ""}]},
            {"<a@b.com>, <c@d.org>", [{{"a","b.com"}, ""},
                                      {{"c","d.org"}, ""}]},
            %% Characters in adresses
            {"<Aa_.+09@aA_90>", [{{"Aa_.+09", "aA_90"}, ""}]},
            %% Free-standing email address
            {"a@b.com", [{{"a","b.com"}, ""}]},
            {"a@b.com, c@d", [{{"a","b.com"}, ""}, {{"c","d"}, ""}]},
            %% Top-level domain address
            {"<fred@com>", [{{"fred","com"}, ""}]},
            %% Surrounding whitespace
            {"\t <abc@def.com> \t", [{{"abc","def.com"}, ""}]},
            {"\t abc@def.com \t", [{{"abc","def.com"}, ""}]},
            {"\t <a@b> \t, <c@d>\t ", [{{"a","b"}, ""},
                                       {{"c","d"}, ""}]},
            %% Quoting in local-part
            {"<\"foo\"@org>",        [{{"foo","org"},""}]},
            {"<\"\\f\\o\\o\"@org>",  [{{"foo","org"},""}]},
            {"<\"\\0\\2\\9\"@org>",  [{{"029","org"},""}]},
            {"<\"\\(\\)\\<\\>\\[\\]\\:\\;\\@\\\\\\,\\.\\\"\"@org>",
             [{{"()<>[]:;@\\,.\"","org"},""}]},
            %% Long address, with dots
            {"<firstname.surname@a.long.domain.name>", [{{"firstname.surname","a.long.domain.name"}, ""}]},
            %% Groups
            {"undisclosed-recipients:;",
             [{group, "undisclosed-recipients", []}]},
            {"<a@b>, undisclosed-recipients:;, <c@d>",
             [{{"a","b"}, ""},
              {group, "undisclosed-recipients", []},
              {{"c","d"}, ""}]},
            {"group1:<foo@bar>;, b@c, group2 : <x@z>, <y@quux>;",
             [{group, "group1", [{{"foo","bar"},""}]},
              {{"b","c"},""},
              {group, "group2", [{{"x","z"},""}, {{"y","quux"},""}]}]},
            {" group1 : <foo@bar>; , b@c , group2 : <x@z> , <y@quux> ; ",
             [{group, "group1", [{{"foo","bar"},""}]},
              {{"b","c"},""},
              {group, "group2", [{{"x","z"},""}, {{"y","quux"},""}]}]},
            %% Examples from RFC5322:
            {"\"Joe Q. Public\" <john.q.public@example.com>",
             [{{"john.q.public","example.com"}, "Joe Q. Public"}]},
            {"Mary Smith <mary@x.test>, jdoe@example.org, Who? <one@y.test>",
             [{{"mary","x.test"},      "Mary Smith"},
              {{"jdoe","example.org"}, ""},
              {{"one","y.test"},      "Who?"}
             ]},
            {"<boss@nil.test>, \"Giant; \\\"Big\\\" Box\" <sysservices@example.net>",
             [{{"boss","nil.test"}, ""},
              {{"sysservices","example.net"}, "Giant; \"Big\" Box"}]}
           ],
    [{Input, % set test title
      ?_assertEqual({ok,Expected}, epop_address:parse_list(Input))}
     || {Input, Expected} <- Spec].

expand_groups_test() ->
    Input = "group1:<foo@bar>;, b@c, group2 : <x@z>, <y@quux>;",
    {ok, Addrs} = epop_address:parse_list(Input),
    Output = (catch epop_address:expand_groups(Addrs)),
    ?assertEqual([{{"foo","bar"},""},
                  {{"b","c"},""},
                  {{"x","z"},""},
                  {{"y","quux"},""}],
                 Output).
