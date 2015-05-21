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

  Warning* Parser::parse_warning()
  {
    return new (mem) Warning(pstate, parse_list());
  }

  Error* Parser::parse_error()
  {
    return new (mem) Error(pstate, parse_list());
  }

  Debug* Parser::parse_debug()
  {
    return new (mem) Debug(pstate, parse_list());
  }

  Return* Parser::parse_return_directive()
  {
    return new (mem) Return(pstate, parse_list());
  }

  Parameters* Parser::parse_parameters()
  {
    string name(lexed);
    Position position = after_token;
    Parameters* params = new (mem) Parameters(pstate);
    if (lex_css< exactly<'('> >()) {
      // if there's anything there at all
      if (!peek_css< exactly<')'> >()) {
        do (*params) << parse_parameter();
        while (lex_css< exactly<','> >());
      }
      if (!lex_css< exactly<')'> >()) error("expected a variable name (e.g. $x) or ')' for the parameter list for " + name, position);
    }
    return params;
  }

  Parameter* Parser::parse_parameter()
  {
    while (lex< alternatives < spaces, block_comment > >());
    lex	< variable >();
    string name(Util::normalize_underscores(lexed));
    ParserState pos = pstate;
    Expression* val = 0;
    bool is_rest = false;
    while (lex< alternatives < spaces, block_comment > >());
    if (lex< exactly<':'> >()) { // there's a default value
      while (lex< block_comment >());
      val = parse_space_list();
      val->is_delayed(false);
    }
    else if (lex< exactly< ellipsis > >()) {
      is_rest = true;
    }
    Parameter* p = new (mem) Parameter(pos, name, val, is_rest);
    return p;
  }


  Argument* Parser::parse_argument(bool has_url)
  {

    Argument* arg;
    // some urls can look like line comments (parse literally - chunk would not work)
    if (has_url && lex< sequence < uri_value, lookahead < loosely<')'> > > >(false)) {
      String* the_url = parse_interpolated_chunk(lexed);
      arg = new (mem) Argument(the_url->pstate(), the_url);
    }
    else if (peek_css< sequence < variable, optional_css_comments, exactly<':'> > >()) {
      lex_css< variable >();
      string name(Util::normalize_underscores(lexed));
      ParserState p = pstate;
      lex_css< exactly<':'> >();
      Expression* val = parse_space_list();
      val->is_delayed(false);
      arg = new (mem) Argument(p, val, name);
    }
    else {
      bool is_arglist = false;
      bool is_keyword = false;
      Expression* val = parse_space_list();
      val->is_delayed(false);
      if (lex_css< exactly< ellipsis > >()) {
        if (val->concrete_type() == Expression::MAP) is_keyword = true;
        else is_arglist = true;
      }
      arg = new (mem) Argument(pstate, val, "", is_arglist, is_keyword);
    }
    return arg;
  }

  Arguments* Parser::parse_arguments(bool has_url)
  {
    string name(lexed);
    Position position = after_token;
    Arguments* args = new (mem) Arguments(pstate);
    if (lex_css< exactly<'('> >()) {
      // if there's anything there at all
      if (!peek_css< exactly<')'> >()) {
        do (*args) << parse_argument(has_url);
        while (lex_css< exactly<','> >());
      }
      if (!lex_css< exactly<')'> >()) error("expected a variable name (e.g. $x) or ')' for the parameter list for " + name, position);
    }
    return args;
  }


  Function_Call* Parser::parse_function_call()
  {
    lex< identifier >();
    string name(lexed);

    ParserState call_pos = pstate;
    Arguments* args = parse_arguments(name == "url");
    return new (mem) Function_Call(call_pos, name, args);
  }


  // calc functions should preserve arguments
  Function_Call* Parser::parse_calc_function()
  {
    lex< identifier >();
    string name(lexed);
    ParserState call_pos = pstate;

    lex< exactly<'('> >();
    ParserState arg_pos = pstate;
    const char* arg_beg = position;
    parse_list();
    const char* arg_end = position;
    lex< skip_over_scopes <
          exactly < '(' >,
          exactly < ')' >
        > >();

    Argument* arg = new (mem) Argument(arg_pos, parse_interpolated_chunk(Token(arg_beg, arg_end)));
    Arguments* args = new (mem) Arguments(arg_pos);
    *args << arg;
    return new (mem) Function_Call(call_pos, name, args);
  }

  Function_Call_Schema* Parser::parse_function_call_schema()
  {
    String* name = parse_identifier_schema();
    ParserState source_position_of_call = pstate;

    Function_Call_Schema* the_call = new (mem) Function_Call_Schema(source_position_of_call, name, parse_arguments());
    return the_call;
  }

  If* Parser::parse_if_directive(bool else_if)
  {
    ParserState if_source_position = pstate;
    Expression* predicate = parse_list();
    predicate->is_delayed(false);
    if (!lex< exactly<'{'> >()) error("expected '{' after the predicate for @if", pstate);
    Block* consequent = parse_block();
    Block* alternative = 0;

    if (lex< elseif_directive >()) {
      alternative = new (mem) Block(pstate);
      (*alternative) << parse_if_directive(true);
    }
    else if (lex< kwd_else_directive >()) {
      if (!lex< exactly<'{'> >()) {
        error("expected '{' after @else", pstate);
      }
      else {
        alternative = parse_block();
      }
    }
    return new (mem) If(if_source_position, predicate, consequent, alternative);
  }

  For* Parser::parse_for_directive()
  {
    ParserState for_source_position = pstate;
    lex_variable();
    string var(Util::normalize_underscores(lexed));
    if (!lex< kwd_from >()) error("expected 'from' keyword in @for directive", pstate);
    Expression* lower_bound = parse_expression();
    lower_bound->is_delayed(false);
    bool inclusive = false;
    if (lex< kwd_through >()) inclusive = true;
    else if (lex< kwd_to >()) inclusive = false;
    else                  error("expected 'through' or 'to' keyword in @for directive", pstate);
    Expression* upper_bound = parse_expression();
    upper_bound->is_delayed(false);
    if (!lex< exactly<'{'> >()) error("expected '{' after the upper bound in @for directive", pstate);
    Block* body = parse_block();
    return new (mem) For(for_source_position, var, lower_bound, upper_bound, body, inclusive);
  }

  // helper to parse a var token
  Token Parser::lex_variable()
  {
    // peek for dollar sign first
    if (!peek< exactly <'$'> >()) {
      css_error("Invalid CSS", " after ", ": expected \"$\", was ");
    }
    // we expect a simple identfier as the call name
    if (!lex< sequence < exactly <'$'>, identifier > >()) {
      lex< exactly <'$'> >(); // move pstate and position up
      css_error("Invalid CSS", " after ", ": expected identifier, was ");
    }
    // return object
    return token;
  }
  // helper to parse identifier
  Token Parser::lex_identifier()
  {
    // we expect a simple identfier as the call name
    if (!lex< identifier >()) { // ToDo: pstate wrong?
      css_error("Invalid CSS", " after ", ": expected identifier, was ");
    }
    // return object
    return token;
  }

  Each* Parser::parse_each_directive()
  {
    ParserState each_source_position = pstate;
    vector<string> vars;
    lex_variable();
    vars.push_back(Util::normalize_underscores(lexed));
    while (lex< exactly<','> >()) {
      if (!lex< variable >()) error("@each directive requires an iteration variable", pstate);
      vars.push_back(Util::normalize_underscores(lexed));
    }
    if (!lex< kwd_in >()) error("expected 'in' keyword in @each directive", pstate);
    Expression* list = parse_list();
    list->is_delayed(false);
    if (list->concrete_type() == Expression::LIST) {
      List* l = static_cast<List*>(list);
      for (size_t i = 0, L = l->length(); i < L; ++i) {
        (*l)[i]->is_delayed(false);
      }
    }
    if (!lex< exactly<'{'> >()) error("expected '{' after the upper bound in @each directive", pstate);
    Block* body = parse_block();
    return new (mem) Each(each_source_position, vars, list, body);
  }


  // called after parsing `kwd_while_directive`
  While* Parser::parse_while_directive()
  {
    // create the initial while call object
    While* call = new (mem) While(pstate, 0, 0);
    // parse mandatory predicate
    Expression* predicate = parse_list();
    predicate->is_delayed(false);
    call->predicate(predicate);
    // parse optional block
    if (lex < exactly <'{'> >()) {
      call->block(parse_block());
    }
    // return ast node
    return call;
  }
  // EO parse_while_directive


  // called after parsing `kwd_include_directive`
  Mixin_Call* Parser::parse_include_directive()
  {
    not_selector = true;
    // lex identifier into `lexed` var
    lex_identifier(); // may error out
    // normalize underscores to hyphens
    string name(Util::normalize_underscores(lexed));
    // create the initial mixin call object
    Mixin_Call* call = new (mem) Mixin_Call(pstate, name, 0, 0);
    // parse mandatory arguments
    call->arguments(parse_arguments());
    // parse optional block
    if (lex < exactly <'{'> >()) {
      call->block(parse_block());
    }
    not_selector = false;
    // return ast node
    return call;
  }
  // EO parse_include_directive


  // used in parse_block_nodes and parse_at_rule
  // ToDo: actual usage is still not really clear to me?
  Lookahead Parser::lookahead_for_include(const char* start)
  {
    // we actually just lookahead for a selector
    Lookahead rv = lookahead_for_selector(start);
    // but the "found" rules are different
    if (const char* p = rv.position) {
      // check for additional abort condition
      if (peek < exactly<';'> >(p)) rv.found = p;
      else if (peek < exactly<'}'> >(p)) rv.found = p;
    }
    // return result
    return rv;
  }
  // EO lookahead_for_include

}
