CFLAGS=-Wall --std=c99 -ggdb -Wextra
CC=gcc ${CFLAGS}

# By default make uses yacc/lex and will actually overwrite source files
# depending on how they're named. Override this behavior.
%.c: %.y
%.c: %.l

lex.yy.o: lex.yy.c conflag.tab.o
	${CC} -c lex.yy.c

lex.yy.c: conflag.l
	flex conflag.l

conflag.tab.c: conflag.y
	bison -t -d -v conflag.y

conflag.tab.o: conflag.tab.c
	${CC} -c conflag.tab.c

conflag.o: conflag.c conflag.h
	${CC} -c conflag.c

conflag: conflag.o lex.yy.o conflag.tab.o
	${CC} -o conflag main.c conflag.o lex.yy.o conflag.tab.o

lex.yy.c conflag.tab.c: conflag.h

clean:
	rm -f *.o lex.yy.c conflag.tab.c conflag.tab.h conflag.output conflag
