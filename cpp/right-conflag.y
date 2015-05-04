%{

#include "conflag.h"
#include "parser.h"
#include <iostream>

int conflaglex(YYSTYPE *, void *);

%}

%language "C++"
%name-prefix "conflag"
%lex-param {void* conflag::yyguts}

%error-verbose
%debug

%start input

%union {
  //TODO: use %define api.value.type variant
  std::string string;
  conflag::aref node;
  std::list<conflag::aref> nodes;
  std::pair<std::string, conflag::aref> statement;
  std::list<std::pair<std::string, conflag::aref>> statements;
  std::list<std::string> names;
}

%token <string> NUMBER STRING NAME
%token <string> STATEMENT

%type <node> input object expression uexpression call literal
%type <nodes> expressions
%type <statement> statement
%type <statements> statements

%left '+'
%left '.'
%nonassoc '(' ')' '[' ']' '{' '}' ',' '_'

%%

/* TODO: Bison suggests left-recursive rules to avoid having to place the
  entire recursion onto the stack during parsing. */
/* TODO: Often times we have a name when we only care about a string.
  as a result we will undoubtedly leak some name allocs. */

input         : statements { conflag::parse_output = std::make_shared<const conflag::ObjectNode>($1); }
              | uexpression { conflag::parse_output = $1; }

statement     : NAME ':' uexpression { $$ = std::make_pair($1, $3); }
              | STRING ':' uexpression { $$ = std::make_pair($1, $3); }

statements    : statement { $$ = std::list<std::pair<std::string, conflag::aref>>(); $$.push_front($1); }
              | statement ',' { $$ = std::list<std::pair<std::string, conflag::aref>>(); $$.push_front($1); }
              | statement ',' statements { $3.push_front($1); $$ = $3; }

literal       : NUMBER { $$ = conflag::make_number_node($1); }
              | STRING { $$ = std::make_shared<const conflag::StringNode>(conflag::String($1)); }

call          : expression expressions { $$ = std::make_shared<const conflag::CallNode>($1, $2); }

expression    : NAME { $$ = std::make_shared<conflag::NameNode>($1); }
              | literal
              | object

uexpression   : call
              | expression

expressions   : expression expressions { $2.push_front($1); $$ = $2; }
              | expression { $$ = std::list<conflag::aref>(); $$.push_front($1); }

object        : '{' statements '}' { $$ = std::make_shared<const conflag::ObjectNode>($2); }
              | '{' '}' { $$ = std::make_shared<const conflag::ObjectNode>(); }


%%

namespace conflag {

void parser::error(const std::string& msg) {
  std::cout << "Syntax error: " << msg << std::endl;
}

} /* namespace conflag */
