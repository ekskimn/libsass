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

  // parse a selector schema that will be evaluated in the eval stage
  // uses a string schema internally to do the actual schema handling
  // in the eval stage we will be re-parse it into an actual selector
  Selector_Schema* Parser::parse_selector_schema(const char* eos, bool at_root)
  {

    // move up to the start
    lex< optional_spaces >();
    const char* i = position;

    // selector schema re-uses string schema implementation
    String_Schema* schema = new (mem) String_Schema(pstate);
    // the selector schema is pretty much just a wrapper for the string schema
    Selector_Schema* selector_schema = new (mem) Selector_Schema(pstate, schema);

    // set some options from parsing context
    selector_schema->media_block(last_media_block);

    // process until end
    while (i < eos)
    {
      // parse interpolants with optional preceding strings
      if (const char* p = find_first_in_interval< exactly<hash_lbrace> >(i, eos))
      {
        // accumulate the preceding segment if the position has advanced
        if (i < p) (*schema) << new (mem) String_Quoted(pstate, string(i, p));
        // check if the interpolation only contains white-space (error out)
        if (peek < sequence < optional_spaces, exactly<rbrace> > >(p+2)) { position = p+2;
          css_error("Invalid CSS", " after ", ": expected expression (e.g. 1px, bold), was ");
        }
        // skip over all nested inner interpolations up to our own delimiter
        const char* j = skip_over_scopes< exactly<hash_lbrace>, exactly<rbrace> >(p+2, eos);
        // pass inner expression to the parser to resolve nested interpolations
        Expression* interpolant = Parser::from_c_str(p+2, j, ctx, mem, pstate).parse_list();
        // set status on the list expression
        interpolant->is_interpolant(true);
        // add to the string schema
        (*schema) << interpolant;
        // advance position
        i = j;
      }
      // no more interpolants have been found
      // add the last segment if there is one
      else
      {
        // make sure to add the last bits of the string up to the end (if any)
        if (i < eos) (*schema) << new (mem) String_Quoted(pstate, string(i, eos));
        // exit loop
        i = eos;
      }
    }
    // EO until eos

    // update position
    position = i;

    // return parsed result
    return selector_schema;

  }
  // EO parse_selector_schema

  // parse a list of complex selectors
  // this is the main entry point for most
  Selector_List* Parser::parse_selector_list(bool at_root)
  {
  	// cerr << "parse sel list " << at_root << endl;
    bool reloop = true;
    To_String to_string(&ctx);
    Selector_List* group = new (mem) Selector_List(pstate);
    group->media_block(last_media_block);
    do {
      reloop = false;
      lex< css_comments >();
      if (peek< class_char < selector_list_delims > >())
        break; // in case there are superfluous commas at the end

      bool in_root = in_at_root || at_root;

      // now parse the complex selector
      Complex_Selector* sel = parse_complex_selector(in_root);

      while (peek_css< exactly<','> >())
      {
        lex< spaces >();
        lex< css_comments >();
        // consume everything up and including the comma speparator
        reloop = lex< exactly<','> >() != 0;
        // remember line break (also between some commas)
        if (!sel->is_delayed()) {
        if (peek_newline()) sel->has_line_feed(true);
        if (sel->tail() && peek_newline()) sel->tail()->has_line_feed(true);
        if (sel->tail() && sel->tail()->head() && peek_newline()) sel->tail()->head()->has_line_feed(true);
        }
        // remember line break (also between some commas)
      }
      (*group) << sel;
    }
    while (reloop);
    while (lex_css< kwd_optional >()) {
      group->is_optional(true);
    }
    return group;
  }
  // EO parse_selector_list

  // a complex selector combines a compound selector with another
  // complex selector, with one of four combinator operations.
  // the compound selector (head) is optional, since the combinator
  // can come first in the whole selector sequence (like `> DIV').
  Complex_Selector* Parser::parse_complex_selector(bool in_root)
  {

    // parse the left hand side
    Compound_Selector* lhs = 0;
    // special case if it starts with combinator ([+~>])
    if (!peek_css< class_char < selector_combinator_ops > >())
    {
      // parse the left hand side
      lhs = parse_compound_selector();
      lhs->has_line_break(peek_newline());
    }

    // parse combinator between lhs and rhs
    Complex_Selector::Combinator combinator;
    if      (lex< exactly<'+'> >()) combinator = Complex_Selector::ADJACENT_TO;
    else if (lex< exactly<'~'> >()) combinator = Complex_Selector::PRECEDES;
    else if (lex< exactly<'>'> >()) combinator = Complex_Selector::PARENT_OF;
    else /* if (lex< zero >()) */   combinator = Complex_Selector::ANCESTOR_OF;

    // source position of a complex selector points to the combinator
    // ToDo: make sure we update pstate for ancestor of (lex < zero >());
    Complex_Selector* sel = new (mem) Complex_Selector(pstate, combinator, lhs);
    // has linfeed after combinator?
    sel->has_line_break(peek_newline());
    // set some options from parsing context
    sel->media_block(last_media_block);

    // check if we got the abort condition (ToDo: optimize)
    if (!peek_css< class_char < complex_selector_delims > >()) {
      // parse next selector in sequence
      sel->tail(parse_complex_selector());
      // ToDo: move the logic below into tail setter
      if (sel->tail()->has_reference()) sel->has_reference(true);
      if (sel->tail()->has_placeholder()) sel->has_placeholder(true);
    }

    // add a parent selector if we are not in a root
    // also skip adding parent ref if we only have refs
    if (!sel->has_reference() && !in_root) {
      // create the objects to wrap parent selector reference
      Parent_Selector* parent = new (mem) Parent_Selector(pstate);
      Compound_Selector* head = new (mem) Compound_Selector(pstate);
      // add simple selector
      (*head) << parent;
      // selector may not have any head yet
      if (!sel->head()) { sel->head(head); }
      // otherwise we need to create a new complex selector and set the old one as its tail
      else { sel = new (mem) Complex_Selector(pstate, Complex_Selector::ANCESTOR_OF, head, sel); }
      // peek for linefeed and remember result on head
      if (peek_newline()) head->has_line_break(true);
    }

    // complex selector
    return sel;

  }
  // EO parse_complex_selector

  // parse one compound selector, which is basically
  // a list of simple selectors (directly adjancent)
  // lex them exactly (without skipping white-space)
  Compound_Selector* Parser::parse_compound_selector()
  {

    // init an empty compound selector wrapper
    Compound_Selector* seq = new (mem) Compound_Selector(pstate);
    // set some options from parsing context
    seq->media_block(last_media_block);

    // skip initial white-space
    lex< css_whitespace >();

    // parse list
    while (true)
    {
      // remove all block comments (don't skip white-space)
      lex< delimited_by< slash_star, star_slash, false > >(false);
      // parse functional
      if (peek < re_pseudo_selector >())
      {
        (*seq) << parse_simple_selector();
      }
      // parse parent selector
      else if (lex< exactly<'&'> >(false))
      {
        (*seq) << new (mem) Parent_Selector(pstate);
      }
      // parse type selector
      else if (lex< re_type_selector >(false))
      {
        (*seq) << new (mem) Type_Selector(pstate, lexed);
      }
      // peek for abort conditions
      else if (peek< spaces >()) break;
      else if (peek_css < class_char < selector_combinator_ops > >()) break;
      else if (peek_css < class_char < complex_selector_delims > >()) break;
      // otherwise parse another simple selector
      else (*seq) << parse_simple_selector();
    }

    // EO while true
    return seq;

  }
  // EO parse_compound_selector

  Simple_Selector* Parser::parse_simple_selector()
  {
    lex < css_comments >();
    if (lex< alternatives < id_name, class_name > >()) {
      return new (mem) Selector_Qualifier(pstate, lexed);
    }
    else if (lex< quoted_string >()) {
      return new (mem) Type_Selector(pstate, unquote(lexed));
    }
    else if (lex< alternatives < variable, number, kwd_sel_deep > >()) {
      return new (mem) Type_Selector(pstate, lexed);
    }
    else if (peek< pseudo_not >()) {
      return parse_negated_selector();
    }
    else if (peek< re_pseudo_selector >()) {
      return parse_pseudo_selector();
    }
    else if (peek< exactly<':'> >()) {
      return parse_pseudo_selector();
    }
    else if (lex< exactly<'['> >()) {
      return parse_attribute_selector();
    }
    else if (lex< placeholder >()) {
      Selector_Placeholder* sel = new (mem) Selector_Placeholder(pstate, lexed);
      sel->media_block(last_media_block);
      return sel;
    }
    else {
      error("invalid selector after " + lexed.to_string(), pstate);
    }
    // unreachable statement
    return 0;
  }

  Wrapped_Selector* Parser::parse_negated_selector()
  {
    lex< pseudo_not >();
    string name(lexed);
    ParserState nsource_position = pstate;
    Selector* negated = parse_selector_list();
    if (!lex< exactly<')'> >()) {
      error("negated selector is missing ')'", pstate);
    }
    name.erase(name.size() - 1);
    return new (mem) Wrapped_Selector(nsource_position, name, negated);
  }

  // a pseudo selector often starts with one or two colons
  // it can contain more selectors inside parantheses
  Simple_Selector* Parser::parse_pseudo_selector()
  {
    if (lex< sequence< optional < pseudo_prefix >,

    	// we keep the space within the name, strange enough
    	// ToDo: refactor output to schedule the space for it
    	// or do we really want to keep the real white-space?
    	sequence< identifier, optional < block_comment >, exactly<'('> >

    	 > >())
    {

      string name(lexed);
      name.erase(name.size() - 1);
      ParserState p = pstate;

      // specially parse static stuff
      // ToDo: really everything static?
      if (peek_css <
            sequence <
              alternatives <
                static_value,
                binomial
              >,
              optional_css_whitespace,
              exactly<')'>
            >
          >()
      ) {
        lex_css< alternatives < static_value, binomial > >();
        String_Constant* expr = new (mem) String_Constant(pstate, lexed);
        if (expr && lex_css< exactly<')'> >()) {
          expr->can_compress_whitespace(true);
          return new (mem) Pseudo_Selector(p, name, expr);
        }
      }
      else if (Selector* wrapped = parse_selector_list()) {
        if (wrapped && lex_css< exactly<')'> >()) {
          return new (mem) Wrapped_Selector(p, name, wrapped);
        }
      }

    }
    // EO if pseudo selector

    else if (lex < sequence< optional < pseudo_prefix >, identifier > >()) {
      return new (mem) Pseudo_Selector(pstate, lexed);
    }
    else if(lex < pseudo_prefix >()) {
      css_error("Invalid CSS", " after ", ": expected pseudoclass or pseudoelement, was ");
    }

    css_error("Invalid CSS", " after ", ": expected \")\", was ");

    // unreachable statement
    return 0;
  }

  Attribute_Selector* Parser::parse_attribute_selector()
  {
    ParserState p = pstate;
    if (!lex_css< attribute_name >()) error("invalid attribute name in attribute selector", pstate);
    string name(lexed);
    if (lex_css< exactly<']'> >()) return new (mem) Attribute_Selector(p, name, "", 0);
    if (!lex_css< alternatives< exact_match, class_match, dash_match,
                                prefix_match, suffix_match, substring_match > >()) {
      error("invalid operator in attribute selector for " + name, pstate);
    }
    string matcher(lexed);

    String* value = 0;
    if (lex_css< identifier >()) {
      value = new (mem) String_Constant(p, lexed);
    }
    else if (lex_css< quoted_string >()) {
      value = parse_interpolated_chunk(lexed, true); // needed!
    }
    else {
      error("expected a string constant or identifier in attribute selector for " + name, pstate);
    }

    if (!lex_css< exactly<']'> >()) error("unterminated attribute selector for " + name, pstate);
    return new (mem) Attribute_Selector(p, name, matcher, value);
  }


  Lookahead Parser::lookahead_for_selector(const char* start)
  {
    // init result struct
    Lookahead rv { 0 };
    // get start position
    const char* p = start ? start : position;
    // match in one big "regex"
    if (const char* q =
      peek <
        one_plus <
          alternatives <
            // consume whitespace and comments
            spaces, block_comment, line_comment,
            // match `/deep/` selector (pass-trough)
            // there is no functionality for it yet
            exactly<sel_deep_kwd>,
            // match selector ops /[*&%,()\[\]]/
            class_char < selector_lookahead_ops >,
            // match selector combinators /[>+~]/
            class_char < selector_combinator_ops >,
            // match attribute compare operators
            alternatives <
              exact_match, class_match, dash_match,
              prefix_match, suffix_match, substring_match
            >,
            // main selector match
            sequence <
              // modifiers prefixes
              alternatives <
                sequence <
                  exactly <'#'>,
                  // not for interpolation
                  negate < exactly <'{'> >
                >,
                // class match
                exactly <'.'>,
                // single or double colon
                optional < pseudo_prefix >
              >,
              // can be namespaced
              optional < namespace_prefix >,
              // accept hypens in token
              one_plus < sequence <
                // can start with hyphens
                zero_plus < exactly<'-'> >,
                // now the main token
                alternatives <
                  kwd_optional,
                  quoted_string,
                  interpolant,
                  identifier,
                  percentage,
                  dimension,
                  variable,
                  alnum
                >
              > >,
              // can also end with hyphens
              zero_plus < exactly<'-'> >
            >
          >
        >
      >(p)
    ) {
      while (p < q) {
        // did we have interpolations?
        if (*p == '#' && *(p+1) == '{') {
          rv.has_interpolants = true;
          p = q; break;
        }
        ++ p;
      }
      // store anyway
      // ToDo: remove
      rv.position = q;
      // check expected opening bracket
      // only after successfull matching
      if (peek < exactly<'{'> >(q)) rv.found = q;
      // else if (peek < exactly<';'> >(q)) rv.found = q;
      // else if (peek < exactly<'}'> >(q)) rv.found = q;
    }

    // return result
    return rv;

  }
  // EO lookahead_for_selector

}
