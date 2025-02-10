# polymlb is also supported
SML ?= mlton

all: lox

lox: $(shell mlton -stop f lox.mlb)
	$(SML) -output $@ lox.mlb
