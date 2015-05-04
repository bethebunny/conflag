#ifndef __conflag_parser_h
#define __conflag_parser_h

#include "conflag.h"

namespace conflag {

struct parser_semantic_type {
  std::string string;
  conflag::aref node;
  std::vector<conflag::aref> nodes;
  std::pair<std::string, conflag::aref> statement;
  std::vector<std::pair<std::string, conflag::aref>> statements;
  std::vector<std::string> names;
};

extern thread_local void *yyguts;

} /* namespace conflag */

#define YYSTYPE conflag::parser_semantic_type

#endif
