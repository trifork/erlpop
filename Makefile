all:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit skip_deps=true

.PHONY: all clean test
