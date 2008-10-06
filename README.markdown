erlpop
============

`erlpop` is a POP3 client library for Erlang. It is derived from the original "epop" Erlang package which includes both a POP server and client.

Author: *Harish Mallipeddi* (harish.mallipeddi@gmail.com)

### Changes ###
    2009-10-06 Added `epop_message` to parse retrieved email messages.
    2008-09-10 Added SSL support (epop_client can be now be used with services like GMail which require SSL).

### Usage ###

    > {ok, Client} = epop_client:connect("yourname@gmail.com", "yourpassword", [{addr,"pop.gmail.com"},{port,995},ssl]).
    > epop_client:stat(Client).
    > epop_client:retrieve(Client, 1).
