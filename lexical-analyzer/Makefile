# Lexer constructor

all: clean lexer full

lex.yy.cc: cool.flex
	flex -o $@ $? 

lexer: lex.yy.cc
	g++ *.cc -o $@

ready: clean lexerReady full

lexReady.yy.cc: coolReady.flex
	flex -o $@ $? 

lexerReady: lexReady.yy.cc
	g++ *.cc -o lexer

# Lexer execution

full: exec compare

exec:
	./lexer examples/test1.cl > examples/test1.output
	./lexer examples/test2.cl > examples/test2.output
	./lexer examples/test3.cl > examples/test3.output

compare:
	-diff examples/test1.cl.out examples/test1.output > examples/comptest1.txt
	-diff examples/test2.cl.out examples/test2.output > examples/comptest2.txt
	-diff examples/test3.cl.out examples/test3.output > examples/comptest3.txt

# Utilities

clean:
	rm -f lex*.yy.cc
	rm -f lexer*
	rm -f examples/*.output
	rm -f examples/*.txt