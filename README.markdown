erlpop
============

POP3 client library for Erlang. It is derived from the original "epop" Erlang package which includes both a POP server and client.

epop author: [*Torbjörn Törnkvist*](https://web.archive.org/web/19990202132504/http://www.serc.rmit.edu.au/~tobbe) when working at Software Engineering Research Center, SERC, in Melbourne

### Documentation ###

[epop 2.9 documentation](https://nico-amsterdam.github.io/erlpop/epop_client.html)

[RFC 1939](https://tools.ietf.org/html/rfc1939)

Note that the proposed standard [RFC 2449](https://tools.ietf.org/html/rfc2449) is NOT supported.

### Changes ###
    2019-01-26 Nico Hoogervorst  - v1.3, added streaming interface. Added NOOP. Stricter bye-(de-)stuffing
    2018-11-09 Nico Hoogervorst  - v1.2, case-insensitive header lookup. Added erlpop as package 'pop3client' in hex.pm
    2017-06-13 Nico Hoogervorst  - Added 'bin_retrieve' get binary data instead of character list to reduce memory consumption
    2016-08-05 Nico Hoogervorst  - Erlang V8.0.2 / OTP 19 upgrade
    2013-04-18 Erik Søe Sørensen - Add an email address parser 'epop_address' (RFC5322)
    2013-04-17 Erik Søe Sørensen - Erlang/OTP 15 upgrade, "TOP" fix; rebarify
    2013-01-31 Wes James         - update ssl start command to current erlang api 
    2009-10-06 Harish Mallipeddi - Added `epop_message` to parse retrieved email messages.
    2008-09-10 Harish Mallipeddi - Added SSL support (epop_client can be now be used with services like GMail which require SSL).

### License ###

[Erlang public license](https://en.wikipedia.org/wiki/Erlang_Public_License) is deprecated in June 2015 and replaced with Apache 2.0 license


### Usage Erlang ###

    erl
    1> User = "yourname@gmail.com".
    2> {ok, Client} = epop_client:connect(User, "yourpassword",
    2>                                    [ {addr, "pop.gmail.com"}, {port, 995}, {user, User}, ssl ] ).
    3> {ok, {TotalCount, TotalSize}} = epop_client:stat(Client).
    4> {ok, TopContent} = epop_client:top(client, 1, 20).
    5> {message, HeaderList, Body} = epop_message:parse(TopContent)
    6> {ok, MailContent} = epop_client:bin_retrieve(Client, 1).
    7> {message, HeaderList, BodyContent} = epop_message:bin_parse(MailContent).
    8> {ok, Date} = epop_message:find_header(HeaderList, <<"Date">>). 
    9> epop_client:quit(Client).

### Usage Elixir ###

    Specially note how easy it is to create an Elixir stream

    iex -S mix
    iex(1)> user = 'yourname@gmail.com'
    iex(2)> {ok, client} = :epop_client.connect(user, 'yourpassword', 
    ...(2)>                                     [ {:addr, 'pop.gmail.com'}, {:port, 995}, {:user, user}, :ssl] )
    iex(3)> {:ok, {total_count, total_size}} = :epop_client.stat(client)
    iex(4)> # read headers and 20 lines of the body of the first email
    iex(5)> {:ok, mail_content} = :epop_client.top(client, 1, 20)
    iex(6)> # separate headers and body
    iex(7)> {:message, header_list, body} = :epop_message.parse(mail_content)
    iex(8)> # create lazy stream resource for the first email
    iex(9)> mail1stream = apply(Stream, :resource, :epop_client.retrieve_resource_functions(client, 1))
    iex(10)> # read from the stream. Example:
    iex(11)> mail1stream |> Enum.find(fn(val) -> String.contains?(val, "Waldo") end)  |> String.trim
    iex(12)> # use the stream again and it will read the whole mail again. For example, write to fie:
    iex(13)> file = File.stream!("mail1.eml", [:write, :delayed_write])
    iex(14)> mail1stream |> Enum.into(file)
    iex(15)> :epop_client.quit(client)
    
### escript example for downloading emails ###

    escript pop3client_downloader.exs
    
    
  *NOTE*: It's important to call epop_client:quit/1 at the end, as it's responsible for closing (tcp/tls) socket.
