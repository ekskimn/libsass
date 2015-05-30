#include "parser.hpp"

namespace Sass {
  using namespace Constants;


  // lexed after `kwd_supports_directive`
  // these are very similar to media blocks
  Supports_Block* Parser::parse_supports_directive()
  {
    // create the ast node object for the support queries
    Supports_Block* query = new (mem) Supports_Block(pstate);
    // now parse the support queries
    query->queries(parse_supports_queries());
    // additional block is mandatory
    if (!lex < exactly <'{'> >()) {
      error("expected '{' in feature query", pstate);
    }
    // parse inner block
    query->block(parse_block());
    // return ast node
    return query;
  }


  // parse multiple queries for supports blocks
  // these are very similar to media queries
  Supports_Query* Parser::parse_supports_queries()
  {
    // lex optional comments
    lex < css_whitespace >();
    // create wrapper object and root condition
    Supports_Query* sq = new (mem) Supports_Query(pstate);
    Supports_Condition* cond = new (mem) Supports_Condition(pstate);
    // first condition is the root
    cond->is_root(true);
    // loop until the abort condition
    while (!peek < exactly <'{'> >())
      (*cond) << parse_supports_query();
    // add condition
    (*sq) << cond;
    // at least one query is mandatory (ToDo: check for ruby sass compat)
    if (sq->empty()) error("expected @supports condition (e.g. (display: flexbox))", pstate);
    if (!peek_css < exactly <'{'> >()) error("expected \"{\" after @supports declaration", pstate);
    // return ast node
    return sq;
  }
  // EO parse_supports_queries


  // parse one query operation
  // may encounter nested queries
  Supports_Condition* Parser::parse_supports_query()
  {
    Supports_Condition* cond = 0;
    // lex optional comments
    lex < css_whitespace >();
    // parse `not` query operator
    if (lex < kwd_not >(position)) {
      cond = parse_supports_query();
      cond->operand(Supports_Condition::NOT);
    }
    // parse `and` query operator
    else if (lex < kwd_and >(position)) {
      cond = parse_supports_query();
      cond->operand(Supports_Condition::AND);
    }
    // parse `or` query operator
    else if (lex < kwd_or >(position)) {
      cond = parse_supports_query();
      cond->operand(Supports_Condition::OR);
    }
    // parse another list with queries
    else if (lex < exactly <'('> >()) {
      // create the inner (parenthesis) condition
      cond = new (mem) Supports_Condition(pstate);
      // parse inner supports queries recursively
      while (!peek < exactly <')'> >())
        (*cond) << parse_supports_query();
      // at least one query is mandatory (ToDo: check for ruby sass compat)
      if (cond->empty()) error("expected @supports condition (e.g. (display: flexbox))", pstate);
      // the parenthesis closer is mandatory (ToDo: check for ruby sass compat)
      if (!lex_css < exactly <')'> >()) error("unclosed parenthesis in @supports declaration", pstate);
      // if we have just one query, we do not wrap it
      cond = (cond->length() == 1) ? (*cond)[0] : cond;
    }
    else {
      // or parse something declaration like
      Declaration* declaration = parse_declaration();
      cond = new (mem) Supports_Condition(declaration->pstate(),
                                          1,
                                          declaration->property(),
                                          declaration->value());
      // ToDo: maybe we need an addition error condition?
    }
    return cond;
  }
  // EO parse_supports_query

}

