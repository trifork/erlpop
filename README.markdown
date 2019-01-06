erlpop
============

POP3 client library for Erlang. It is derived from the original "epop" Erlang package which includes both a POP server and client.

epop author: [*Torbjörn Törnkvist*](https://web.archive.org/web/19990202132504/http://www.serc.rmit.edu.au/~tobbe) when working at Software Engineering Research Center, SERC, in Melbourne

### Documentation ###

[epop 2.9 documentation](https://nico-amsterdam.github.io/erlpop/epop_client.html)

[RFC 1939](https://tools.ietf.org/html/rfc1939)

Note that the proposed standard [RFC 2449](https://tools.ietf.org/html/rfc2449) is NOT supported.

### Changes ###
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


### Usage ###

    erl
    1> User = "yourname@gmail.com".
    2> {ok, Client} = epop_client:connect(User, "yourpassword",
    2>                                    [ {addr, "pop.gmail.com"}, {port, 995}, {user, User}, ssl ] ).
    3> {ok, {TotalCount, TotalSize}} = epop_client:stat(Client).
    4> {ok, MailContent} = epop_client:bin_retrieve(Client, 1).
    5> {message, HeaderList, BodyContent} = epop_message:bin_parse(MailContent).
    6> {ok, Date} = epop_message:find_header(HeaderList, <<"Date">>). 
    7> epop_client:quit(Client).
    
  *NOTE*: It's important to call epop_client:quit/1 at the end, as it's responsible for closing (tcp/tls) socket.
