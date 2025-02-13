all: lox

lox: $(shell mlton -stop f lox.mlb)
	mlton -output $@ lox.mlb
