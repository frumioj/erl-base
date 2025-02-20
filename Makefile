.PHONY: all compile clean test dialyzer shell check

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit

dialyzer:
	rebar3 dialyzer

shell:
	rebar3 shell

# Add a check target that runs multiple verifications
check: compile test dialyzer 