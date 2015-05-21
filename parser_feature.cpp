#include <cstdlib>
#include <iostream>
#include <vector>
#include "parser.hpp"
#include "file.hpp"
#include "inspect.hpp"
#include "to_string.hpp"
#include "constants.hpp"
#include "util.hpp"
#include "prelexer.hpp"
#include "sass_functions.h"

#include <typeinfo>

namespace Sass {
  using namespace std;
  using namespace Constants;


  Feature_Block* Parser::parse_feature_block()
  {
    ParserState supports_source_position = pstate;

    Feature_Query* feature_queries = parse_feature_queries();

    if (!lex< exactly<'{'> >()) {
      error("expected '{' in feature query", pstate);
    }
    Block* block = parse_block();

    return new (mem) Feature_Block(supports_source_position, feature_queries, block);
  }

  Feature_Query* Parser::parse_feature_queries()
  {
    Feature_Query* fq = new (mem) Feature_Query(pstate);
    Feature_Query_Condition* cond = new (mem) Feature_Query_Condition(pstate);
    cond->is_root(true);
    while (!peek< exactly<')'> >(position) && !peek< exactly<'{'> >(position))
      (*cond) << parse_feature_query();
    (*fq) << cond;

    if (fq->empty()) error("expected @supports condition (e.g. (display: flexbox))", pstate);

    return fq;
  }

  Feature_Query_Condition* Parser::parse_feature_query()
  {
    Feature_Query_Condition* cond = 0;
    if (lex< kwd_not >(position)) {
      cond = parse_feature_query();
      cond->operand(Feature_Query_Condition::NOT);
    }
    else if (lex< kwd_and >(position)) {
      cond = parse_feature_query();
      cond->operand(Feature_Query_Condition::AND);
    }
    else if (lex< kwd_or >(position)) {
      cond = parse_feature_query();
      cond->operand(Feature_Query_Condition::OR);
    }
    else if (lex< exactly<'('> >(position)) {
      cond = new (mem) Feature_Query_Condition(pstate);
      while (!peek< exactly<')'> >(position) && !peek< exactly<'{'> >(position))
        (*cond) << parse_feature_query();
      if (!lex< exactly<')'> >()) error("unclosed parenthesis in @supports declaration", pstate);
      cond = (cond->length() == 1) ? (*cond)[0] : cond;
    }
    else {
      Declaration* declaration = parse_declaration();
      cond = new (mem) Feature_Query_Condition(declaration->pstate(),
                                               1,
                                               declaration->property(),
                                               declaration->value());
    }
    return cond;
  }

}

