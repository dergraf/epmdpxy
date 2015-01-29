REBAR=$(shell which rebar)

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
	$(error "Erlang not available on this system")
endif 

ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif

all: compile test

compile:
	$(REBAR) skip_deps=true compile

eunit: compile
	ERL_FLAGS="-epmd_port 43690" $(REBAR) skip_deps=true eunit

test: compile eunit

clean:
	- rm -rf $(CURDIR)/.eunit
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean
