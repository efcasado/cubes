###========================================================================
### File: Makefile
###
### A simple Makefile that builds the cubes project
###
### Usage: make <target>
###        <target> := all | compile | test | clean
###
### Author: Enrique Fernandez <efcasado@gmail.com>
### Date:   February, 2015
###========================================================================
ERL          := $(shell which erl)
ERLC         := $(shell which erlc)

ERLC_OPTS      :=
TEST_ERLC_OPTS := -DTEST

SRC_DIR      := src
BIN_DIR      := ebin
TST_DIR      := test
TEST_BIN_DIR := $(TST_DIR)/$(BIN_DIR)

SRC_FILES      = $(notdir $(shell find $(SRC_DIR) -type f -name *.erl))
BIN_FILES      = $(patsubst %.erl,$(BIN_DIR)/%.beam,$(SRC_FILES))
TEST_BIN_FILES = $(patsubst %.erl,$(TEST_BIN_DIR)/%.beam,$(SRC_FILES))
VPATH          = $(SRC_DIR)

all: compile

compile: $(BIN_DIR) $(BIN_FILES)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(BIN_DIR)/%.beam: %.erl
	$(ERLC) -o $(BIN_DIR) $(ERLC_OPTS) $<

test: $(TEST_BIN_DIR) $(TEST_BIN_FILES) eunit

eunit:
	@$(ERL) -pa $(TEST_BIN_DIR) -noshell -pa $(TEST_BIN_DIR) -eval \
'case eunit:test({dir,"$(TEST_BIN_DIR)"},[verbose]) of ok -> erlang:halt(0); \
_ -> erlang:halt(1) end.'

$(TEST_BIN_DIR):
	mkdir -p $(TEST_BIN_DIR)

$(TEST_BIN_DIR)/%.beam: %.erl
	$(ERLC) -o $(TEST_BIN_DIR) $(TEST_ERLC_OPTS) $<

clean:
	rm -rf $(BIN_DIR)
	rm -rf $(TEST_BIN_DIR)
