%{

#include "conflag.h"

void yyerror(const char*);

%}

%language "c"

%define parse.lac full

%error-verbose
%debug

%start input

%union {
  char* raw_string;
  conflag_string* string;
  conflag_ast_node node;
  conflag_ast_nodes* nodes;
  conflag_ast_statement* statement;
  conflag_ast_names* names;
}

%token <raw_string> NUMBER
%token <string> STRING NAME

%type <node> input object name expression call lambda literal
%type <nodes> expressions array
%type <statement> statement statements function
%type <names> names
%type <string> string

%left '+'
%left '.'
%nonassoc '(' ')' '[' ']' '{' '}' ',' '_'

%%

/* TODO: Bison suggests left-recursive rules to avoid having to place the
  entire recursion onto the stack during parsing. */
/* TODO: Often times we have a name when we only care about a string.
  as a result we will undoubtedly leak some name allocs. */

input:        statements { conflag_parse_output = conflag_ast_object_new($1); }
              | object { conflag_parse_output = $1; }
              | expression { conflag_parse_output = $1; }

object:       '{' statements '}' { $$ = conflag_ast_object_new($2); }
              | '{' '}' { $$ = conflag_ast_object_new(NULL); }

statements:   statement ',' statements { $1->next = $3; $$ = $1; }
              | statement ','
              | statement

statement:    function
              | name ':' expression { $$ = conflag_ast_statement_new($1.name, $3); }
              | string ':' expression { $$ = conflag_ast_statement_new($1, $3); }

names:        name names { $$ = conflag_ast_names_new($1, $2); }
              | name { $$ = conflag_ast_names_new($1, NULL); }

name:         NAME { $$ = conflag_ast_name_new($1); }

expression:   name
              | call
              | literal
              | object
              | '[' array ']' { $$ = conflag_ast_array_new($2); }
              | '(' expression ')' { $$ = $2; }
              | '(' lambda ')' { $$ = $2; }
              | expression '.' name { $$ = conflag_ast_reference_new($1, $3.name); }
              | expression '.' string { $$ = conflag_ast_reference_new($1, $3); }
              | expression '+' expression { $$ = conflag_ast_plus_new($1, $3); }

expressions:  expression expressions { $$ = conflag_ast_nodes_new($1, $2); }
              | expression { $$ = conflag_ast_nodes_new($1, NULL); }

call:         expression expressions { $$ = conflag_ast_call_new($1, $2); }

function:     name names ':' expression { $$ = conflag_ast_function_new($1.name, $2, $4); }
              | string names ':' expression { $$ = conflag_ast_function_new($1, $2, $4); }

lambda:       '_' names ':' expression { $$ = conflag_ast_lambda_new($2, $4); }

array:        expression ',' array { $$ = conflag_ast_nodes_new($1, $3); }
              | expression { $$ = conflag_ast_nodes_new($1, NULL); }
              | /* empty */ { $$ = NULL; }

literal:      NUMBER { $$ = conflag_ast_number_new($1); }
              | string { $$ = (conflag_ast_node) {CONFLAG_AST_STRING, {.string = $1}}; }

string:       STRING


%%

void yyerror(const char *message) {
  printf("parse error: %s\n", message);
}
