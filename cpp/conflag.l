%{
  #include "conflag.h"
  #include "parser.h"
  #include "conflag.tab.hh"
  #include <cstdio>

  #define YY_NO_INPUT
%}

%x IN_BLOCKCOMMENT IN_COMMENT IN_STRING
%option noyywrap nounput

%option prefix="conflag"
%option reentrant
%option bison-bridge

%%

[,.:(){}\[\]_+] { return *yytext; }

"-"?[0-9]+("."[0-9]+)?([eE][0-9]+)? {
  yylval->string = yytext;
  return conflag::parser::token::NUMBER;
}

([A-Za-z_][A-Za-z_0-9]+)|([a-zA-Z])  {
  yylval->string = yytext;
  return conflag::parser::token::NAME;
}

"\"" { BEGIN(IN_STRING); }
<IN_STRING>\\\"|[^\"\n] { yymore(); }
<IN_STRING>"\n" { printf("Unterminated string on line %d", yylineno); yyterminate(); }
<IN_STRING>"\"" {
  BEGIN(INITIAL);
  yytext[yyleng-1] = '\0'; // We don't want the final "
  yylval->string = yytext;
  return conflag::parser::token::STRING;
}

"//" { BEGIN(IN_COMMENT); }
<IN_COMMENT>[\n] { BEGIN(INITIAL); }
<IN_COMMENT>. ;

"/*" { BEGIN(IN_BLOCKCOMMENT); }
<IN_BLOCKCOMMENT>"*/" { BEGIN(INITIAL); }
<IN_BLOCKCOMMENT>.|\n ;

[ \t\r\n]+ ; // ignore whitespace

%%

namespace conflag {

// Needs access to buffer types as well as YY_BUF_SIZE, only defined here
bref load(std::string filename) {
  FILE *file = fopen(filename.c_str(), "r");
  conflaglex_init(&yyguts);
  struct yyguts_t *guts = (struct yyguts_t*) yyguts;
  auto buffer = conflag_create_buffer(file, YY_BUF_SIZE, guts);
  guts->yyin_r = file;
  parser p;
  //p.set_debug_level(1);
  p.parse();
  conflag_delete_buffer(buffer, guts);
  if (!parse_output) { return nullptr; }
  return parse_output->node_value(builtins())->value();
}

} /* namespace conflag */
