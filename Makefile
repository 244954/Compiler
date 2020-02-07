all:
	bison -d parser.y
	flex lexer.l
	g++ parser.tab.c lex.yy.c -o kompilator -lm -std=c++11