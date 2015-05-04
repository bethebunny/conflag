%{
  #include "conflag.h"
  #include "conflag.tab.h"

  int fileno(FILE*);
  #define YY_NO_INPUT
%}

%x IN_BLOCKCOMMENT IN_COMMENT IN_STRING
%option noyywrap nounput

%%

[,.:(){}\[\]_+] { return *yytext; }

"-"?[0-9]+("."[0-9]+)?([eE][0-9]+)? {
  yylval.raw_string = yytext;
  return NUMBER;
}

([A-Za-z_][A-Za-z_0-9]+)|([a-zA-Z])  {
  yylval.string = conflag_string_from_cstr(yytext, yyleng);
  return NAME;
}

"\"" { BEGIN(IN_STRING); }
<IN_STRING>\\\"|[^\"\n] { yymore(); }
<IN_STRING>"\n" { printf("Unterminated string on line %d", yylineno); yyterminate(); }
<IN_STRING>"\"" {
  BEGIN(INITIAL);
  yylval.string = conflag_string_from_cstr(yytext, yyleng - 1);
  return STRING;
}

"//" { BEGIN(IN_COMMENT); }
<IN_COMMENT>[\n] { BEGIN(INITIAL); }

"/*" { BEGIN(IN_BLOCKCOMMENT); }
<IN_BLOCKCOMMENT>"*/" { BEGIN(INITIAL); }

[ \t\r\n]+ ; // ignore whitespace
