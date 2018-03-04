all: deps

compile: deps
	./compile.sh

deps:
	./install-z3.sh
	touch deps

test: compile
	./run_tests.sh

clean:
	rm -rf .stack-work

.PHONY: all compile test clean reallyclean
