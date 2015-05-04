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
  std::vector<conflag::aref> nodes;
  std::pair<std::string, conflag::aref> statement;
  std::vector<std::pair<std::string, conflag::aref>> statements;
  std::vector<std::string> names;
}

%token <string> NUMBER STRING NAME

%type <node> input object expression uexpression call lambda literal
%type <nodes> expressions array
%type <statement> statement function
%type <statements> statements
%type <names> names

%left '+'
%left '.'
%nonassoc '(' ')' '[' ']' '{' '}' ',' '_'

%%

/* TODO: Bison suggests left-recursive rules to avoid having to place the
  entire recursion onto the stack during parsing. */
/* TODO: Often times we have a name when we only care about a string.
  as a result we will undoubtedly leak some name allocs. */

literal       : NUMBER { $$ = conflag::make_number_node($1); }
              | STRING { $$ = std::make_shared<const conflag::StringNode>($1); }

names         : names NAME { $1.push_back($2); $$ = $1; }
              | NAME { $$ = std::vector<std::string>(); $$.push_back($1); }

call          : expressions { auto c = $1[0]; $1.erase($1.begin()); $$ = std::make_shared<const conflag::CallNode>(c, $1); }

expression    : NAME { $$ = std::make_shared<conflag::NameNode>($1); }
              | literal
              | object
              | '[' array ']' { $$ = std::make_shared<const conflag::ArrayNode>($2); }
              | '[' array ',' ']' { $$ = std::make_shared<const conflag::ArrayNode>($2); }
              | '[' ']' { $$ = std::make_shared<const conflag::ArrayNode>(); }
              | '(' uexpression ')' { $$ = $2; }
              | '(' lambda ')' { $$ = $2; }
              | expression '.' NAME { $$ = std::make_shared<const conflag::AccessNode>($1, $3); }
              | expression '.' STRING { $$ = std::make_shared<const conflag::AccessNode>($1, $3); }
              | expression '+' expression { $$ = std::make_shared<const conflag::PlusNode>($1, $3); }

expressions   : expressions expression { $$ = $1; $$.push_back($2); }
              | expression expression { $$ = std::vector<conflag::aref>(); $$.push_back($1); $$.push_back($2); }

uexpression   : call
              | expression

function      : NAME names ':' uexpression { $$ = std::make_pair($1, std::make_shared<const conflag::FunctionNode>($1, $2, $4)); }
              | STRING names ':' uexpression { $$ = std::make_pair($1, std::make_shared<const conflag::FunctionNode>($1, $2, $4)); }

lambda        : '_' names ':' uexpression { $$ = std::make_shared<const conflag::FunctionNode>("_", $2, $4); }

array         : array ',' uexpression { $$ = $1; $$.push_back($3); }
              | uexpression { $$ = std::vector<conflag::aref>(); $$.push_back($1); }

object        : '{' statements '}' { $$ = std::make_shared<const conflag::ObjectNode>($2); }
              | '{' statements ',' '}' { $$ = std::make_shared<const conflag::ObjectNode>($2); }
              | '{' '}' { $$ = std::make_shared<const conflag::ObjectNode>(); }

statements    : statement { $$ = std::vector<std::pair<std::string, conflag::aref>>(); $$.push_back($1); }
              | statements ',' statement { $1.push_back($3); $$ = $1; }

statement     : function
              | NAME ':' uexpression { $$ = std::make_pair($1, $3); }
              | STRING ':' uexpression { $$ = std::make_pair($1, $3); }

input         : statements { conflag::parse_output = std::make_shared<const conflag::ObjectNode>($1); }
              | statements ',' { conflag::parse_output = std::make_shared<const conflag::ObjectNode>($1); }
              | object { conflag::parse_output = $1; }
              | '(' uexpression ')' { conflag::parse_output = $2; }


%%

namespace conflag {

void parser::error(const std::string& msg) {
  std::cout << "Syntax error: " << msg << std::endl;
}

} /* namespace conflag */
