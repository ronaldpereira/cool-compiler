
ASSN = 3
CLASS= cs143
CLASSDIR= ../..
LIB= 
AR= gar
ARCHIVE_NEW= -cr
RANLIB= gar -qs

SRC= cool.y cool-tree.handcode.h good.cl bad.cl README
CSRC= parser-phase.cc utilities.cc stringtab.cc dumptype.cc \
      tree.cc cool-tree.cc tokens-lex.cc  handle_flags.cc 
TSRC= myparser mycoolc cool-tree.aps
CGEN= cool-parse.cc
HGEN= cool-parse.h
LIBS= lexer semant cgen
CFIL= ${CSRC} ${CGEN}
HFIL= cool-tree.h cool-tree.handcode.h 
LSRC= Makefile
OBJS= ${CFIL:.cc=.o}
OUTPUT= good.output bad.output


CPPINCLUDE= -I. -I${CLASSDIR}/include/PA${ASSN} -I${CLASSDIR}/src/PA${ASSN}

BFLAGS = -d -v -y -b cool --debug -p cool_yy

CC=g++
CFLAGS=-g -Wall -Wno-unused -Wno-deprecated  -Wno-write-strings -DDEBUG ${CPPINCLUDE}
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}
DEPEND = ${CC} -MM ${CPPINCLUDE}

source: ${SRC} ${TSRC} ${LIBS} lsource

lsource: ${LSRC}

${OUTPUT}:	parser good.cl bad.cl
	@rm -f ${OUTPUT}
	@./myparser good.cl >good.output 2>&1 
	@-./myparser bad.cl >bad.output 2>&1 

parser: ${OBJS}
	@${CC} ${CFLAGS} ${OBJS} ${LIB} -o parser

.cc.o:
	@${CC} ${CFLAGS} -c $<

cool-parse.cc cool-parse.h: cool.y
	@bison ${BFLAGS} cool.y
	@mv -f cool.tab.c cool-parse.cc

dotest:	parser good.cl bad.cl
	@echo "\nRunning parser on good.cl\n"
	@-./myparser good.cl 
	@echo "\nRunning parser on bad.cl\n"
	@-./myparser bad.cl

${LIBS}:
	@${CLASSDIR}/etc/link-object ${ASSN} $@

${TSRC} ${CSRC}:
	@-ln -s ${CLASSDIR}/src/PA${ASSN}/$@ $@

${HSRC}:
	@-ln -s ${CLASSDIR}/include/PA${ASSN}/$@ $@

clean :
	@-rm -f ${OUTPUT} *.s core ${OBJS} ${CGEN} ${HGEN} lexer parser cgen semant *~ *.a *.o
	@-rm ../../examples/*.s
	@-rm ../../examples/*_output.txt

clean-compile:
	@-rm -f core ${OBJS} ${CGEN} ${HGEN} ${LSRC}

%.d: %.cc ${LSRC}
	@${SHELL} -ec '${DEPEND} $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include ${CFIL:.cc=.d}

test:
	@make clean
	@make parser
	@make
	@./myparser ../../examples/arith.cl > ../../examples/arith_output.txt && ./mycoolc ../../examples/arith.cl
	@./myparser ../../examples/atoi_test.cl > ../../examples/atoi_test_output.txt && ./mycoolc ../../examples/atoi_test.cl ../../examples/atoi.cl
	@./myparser ../../examples/book_list.cl > ../../examples/book_list_output.txt && ./mycoolc ../../examples/book_list.cl
	@./myparser ../../examples/cells.cl > ../../examples/cells_output.txt && ./mycoolc ../../examples/cells.cl
	@./myparser ../../examples/complex.cl > ../../examples/complex_output.txt && ./mycoolc ../../examples/complex.cl
	@./myparser ../../examples/cool.cl > ../../examples/cool_output.txt && ./mycoolc ../../examples/cool.cl
	@./myparser ../../examples/graph.cl > ../../examples/graph_output.txt && ./mycoolc ../../examples/graph.cl
	@./myparser ../../examples/hairyscary.cl > ../../examples/hairyscary_output.txt && ./mycoolc ../../examples/hairyscary.cl
	@./myparser ../../examples/hello_world.cl > ../../examples/hello_world_output.txt && ./mycoolc ../../examples/hello_world.cl
	@./myparser ../../examples/io.cl > ../../examples/io_output.txt && ./mycoolc ../../examples/io.cl
	@./myparser ../../examples/lam.cl > ../../examples/lam_output.txt && ./mycoolc ../../examples/lam.cl
	@./myparser ../../examples/life.cl > ../../examples/life_output.txt && ./mycoolc ../../examples/life.cl
	@./myparser ../../examples/list.cl > ../../examples/list_output.txt && ./mycoolc ../../examples/list.cl
	@./myparser ../../examples/new_complex.cl > ../../examples/new_complex_output.txt && ./mycoolc ../../examples/new_complex.cl
	@./myparser ../../examples/palindrome.cl > ../../examples/palindrome_output.txt && ./mycoolc ../../examples/palindrome.cl
	@./myparser ../../examples/primes.cl > ../../examples/primes_output.txt && ./mycoolc ../../examples/primes.cl
	@./myparser ../../examples/sort_list.cl > ../../examples/sort_list_output.txt && ./mycoolc ../../examples/sort_list.cl
