erlpop
============

POP3 client library for Erlang. It is derived from the original "epop" Erlang package which includes both a POP server and client.

epop author: *Torbjörn Törnkvist* when working at Software Engineering Research Center, SERC, in Melbourne

### Changes ###
    2017-11-09 Nico Hoogervorst  - v1.3.1 rename to pop3client for unique name in hex.pm
    2017-06-13 Nico Hoogervorst  - Added 'bin_retrieve' get binary data instead of character list to reduce memory consumption
    2016-08-05 Nico Hoogervorst  - Erlang V8.0.2 / OTP 19 upgrade
    2013-04-17 Erik Søe Sørensen - Erlang/OTP 15 upgrade, "TOP" fix; rebarify
    2013-01-31 Wes James         - update ssl start command to current erlang api 
    2009-10-06 Harish Mallipeddi - Added `epop_message` to parse retrieved email messages.
    2008-09-10 Harish Mallipeddi - Added SSL support (epop_client can be now be used with services like GMail which require SSL).

### Usage ###

    > {ok, Client} = epop_client:connect("yourname@gmail.com", "yourpassword", [{addr,"pop.gmail.com"},{port,995},ssl]).
    > epop_client:stat(Client).
    > epop_client:retrieve(Client, 1).
