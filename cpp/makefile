FLAGS=-Wall -ggdb -Wextra
CPPFLAGS=--std=c++14 ${FLAGS}
CFLAGS=--std=c99 ${FLAGS}
CC=gcc ${CFLAGS}
CPP=/usr/local/bin/g++ ${CPPFLAGS}

# By default make uses yacc/lex and will actually overwrite source files
# depending on how they're named. Override this behavior.
%.c: %.y
%.c: %.l

lex.conflag.o: lex.conflag.c conflag.tab.o
	${CPP} -c lex.conflag.c

lex.conflag.c: conflag.l conflag.tab.cc
	flex conflag.l

conflag.tab.cc: conflag.y
	bison -t -d -v conflag.y

conflag.tab.o: conflag.tab.cc
	${CPP} -c conflag.tab.cc

conflag.o: conflag.cpp conflag.h conflag.tab.cc
	${CPP} -c conflag.cpp

conflag: main.cpp conflag.o lex.conflag.o conflag.tab.o
	${CPP} -o conflag main.cpp conflag.o lex.conflag.o conflag.tab.o

lex.conflag.c conflag.tab.cc: conflag.h

clean:
	rm -f *.o lex.conflag.c conflag.tab.cc conflag.tab.hh stack.hh conflag.output conflag
