all: dev

.PHONY: dev

dev:
	ENV="dev" rebar3 as dev shell

.PHONY: test

test:
	ENV="test" rebar3 as test do ct, eunit

.PHONY: ct

ct:
	ENV="test" rebar3 as test ct

.PHONY: eunit

eunit:
	ENV="test" rebar3 as test eunit

.PHONY: daemon

# Useful to test the server during development.
daemon:
	ENV="dev" rebar3 as dev shell --eval "sync:go()."
