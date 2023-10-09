all: dev

.PHONY: dev

dev:
	rebar3 as dev shell

.PHONY: test

test:
	rebar3 as test do ct, eunit

.PHONY: daemon

# Useful to test the server during development.
daemon:
	rebar3 as dev shell --eval "sync:go()."
