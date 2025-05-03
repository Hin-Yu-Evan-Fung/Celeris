EXE := Celeris
DEBUG_EXE := Celeris_Debug
DIR := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))/build

ifeq ($(OS),Windows_NT)
	NAME := $(EXE).exe
else
	NAME := $(EXE)
endif

ifeq ($(OS),Windows_NT)
	DEBUG_NAME := $(DEBUG_EXE).exe
else
	DEBUG_NAME := $(DEBUG_EXE)
endif

FEATURES_ARG :=
ifneq ($(strip $(features)),)
FEATURES_ARG := --features $(features)
endif

build:
	cargo clean
	cargo rustc --release --package engine --bin engine $(FEATURES_ARG) -- -C target-cpu=native --emit link=$(NAME)

debug:
	cargo clean
	cargo rustc --package engine --bin engine $(FEATURES_ARG) -- -C target-cpu=native --emit link=$(DEBUG_NAME)

dir:
	mkdir -p $(DIR)

clean:
	rm -fr $(DIR)
	rm -f *.pdb

release: dir
	cargo rustc --release --package engine --bin engine $(FEATURES_ARG) -- -C target-cpu=native -C profile-generate=$(DIR) --emit link=$(NAME)
	./$(NAME) bench
	llvm-profdata merge -o $(DIR)/merged.profdata $(DIR)
	cargo rustc --release --package engine --bin engine $(FEATURES_ARG) -- -C target-feature=+crt-static -C target-cpu=native -C profile-use=$(DIR)/merged.profdata --emit link=$(NAME)
