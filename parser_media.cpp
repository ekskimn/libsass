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


  Media_Block* Parser::parse_media_block()
  {
    ParserState media_source_position = pstate;

    List* media_queries = parse_media_queries();

    if (!lex< exactly<'{'> >()) {
      error("expected '{' in media query", pstate);
    }
    Media_Block* media_block = new (mem) Media_Block(media_source_position, media_queries, 0);
    Media_Block* prev_media_block = last_media_block;
    last_media_block = media_block;
    media_block->block(parse_block());
    last_media_block = prev_media_block;

    return media_block;
  }

  List* Parser::parse_media_queries()
  {
    List* media_queries = new (mem) List(pstate, 0, List::COMMA);
    if (!peek< exactly<'{'> >()) (*media_queries) << parse_media_query();
    while (lex< exactly<','> >()) (*media_queries) << parse_media_query();
    return media_queries;
  }

  // Expression* Parser::parse_media_query()
  Media_Query* Parser::parse_media_query()
  {
    Media_Query* media_query = new (mem) Media_Query(pstate);

    if (lex< exactly< not_kwd > >()) media_query->is_negated(true);
    else if (lex< exactly< only_kwd > >()) media_query->is_restricted(true);

    if (lex< identifier_schema >()) media_query->media_type(parsing_identifier_schema());
    else if (lex< identifier >())    media_query->media_type(parse_interpolated_chunk(lexed));
    else                             (*media_query) << parse_media_expression();

    while (lex< exactly< and_kwd > >()) (*media_query) << parse_media_expression();
    if (peek< identifier_schema >()) {
      String_Schema* schema = new (mem) String_Schema(pstate);
      *schema << media_query->media_type();
      *schema << new (mem) String_Constant(pstate, " ");
      *schema << parse_identifier_schema();
      media_query->media_type(schema);
    }
    while (lex< exactly< and_kwd > >()) (*media_query) << parse_media_expression();
    return media_query;
  }

  Media_Query_Expression* Parser::parse_media_expression()
  {
    if (lex< identifier_schema >()) {
      String* ss = parsing_identifier_schema();
      return new (mem) Media_Query_Expression(pstate, ss, 0, true);
    }
    if (!lex< exactly<'('> >()) {
      error("media query expression must begin with '('", pstate);
    }
    Expression* feature = 0;
    if (peek< exactly<')'> >()) {
      error("media feature required in media query expression", pstate);
    }
    feature = parse_expression();
    Expression* expression = 0;
    if (lex< exactly<':'> >()) {
      expression = parse_list();
    }
    if (!lex< exactly<')'> >()) {
      error("unclosed parenthesis in media query expression", pstate);
    }
    return new (mem) Media_Query_Expression(feature->pstate(), feature, expression);
  }

}

