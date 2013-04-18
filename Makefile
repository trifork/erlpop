all:
	./rebar compile

clean:
	./rebar clean

test:
	./rebar eunit skip_deps=true

.PHONY: all clean test
