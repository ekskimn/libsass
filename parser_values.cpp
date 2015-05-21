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


  Assignment* Parser::parse_assignment()
  {
    string name(Util::normalize_underscores(lexed));
    ParserState var_source_position = pstate;
    if (!lex< exactly<':'> >()) error("expected ':' after " + name + " in assignment statement", pstate);
    Expression* val;
    Lookahead lookahead = lookahead_for_value(position);
    if (lookahead.has_interpolants && lookahead.found) {
      val = parse_value_schema(lookahead.found);
    } else {
      val = parse_list();
    }
    val->is_delayed(false);
    bool is_default = false;
    bool is_global = false;
    while (peek< default_flag >() || peek< global_flag >()) {
      is_default = lex< default_flag >() || is_default;
      is_global = lex< global_flag >() || is_global;
    }
    Assignment* var = new (mem) Assignment(var_source_position, name, val, is_default, is_global);
    return var;
  }

  // parse +/- and return false if negative
  bool Parser::parse_number_prefix()
  {
    bool positive = true;
    while(true) {
      if (lex < block_comment >()) continue;
      if (lex < number_prefix >()) continue;
      if (lex < exactly < '-' > >()) {
        positive = !positive;
        continue;
      }
      break;
    }
    return positive;
  }

  Expression* Parser::parse_map()
  {
    Expression* key = parse_list();
    Map* map = new (mem) Map(pstate, 1);
    if (String_Quoted* str = dynamic_cast<String_Quoted*>(key)) {
      if (!str->quote_mark() && !str->is_delayed()) {
        if (ctx.names_to_colors.count(str->value())) {
          Color* c = new (mem) Color(*ctx.names_to_colors[str->value()]);
          c->pstate(str->pstate());
          c->disp(str->value());
          key = c;
        }
      }
    }

    // it's not a map so return the lexed value as a list value
    if (!peek< exactly<':'> >())
    { return key; }

    lex< exactly<':'> >();

    Expression* value = parse_space_list();

    (*map) << make_pair(key, value);

    while (lex_css< exactly<','> >())
    {
      // allow trailing commas - #495
      if (peek_css< exactly<')'> >(position))
      { break; }

      Expression* key = parse_list();
      if (String_Quoted* str = dynamic_cast<String_Quoted*>(key)) {
        if (!str->quote_mark() && !str->is_delayed()) {
          if (ctx.names_to_colors.count(str->value())) {
            Color* c = new (mem) Color(*ctx.names_to_colors[str->value()]);
            c->pstate(str->pstate());
            c->disp(str->value());
            key = c;
          }
        }
      }

      if (!(lex< exactly<':'> >()))
      { error("invalid syntax", pstate); }

      Expression* value = parse_space_list();

      (*map) << make_pair(key, value);
    }

    // Check was moved to eval step
    // if (map->has_duplicate_key()) {
    //   To_String to_string(&ctx);
    //   error("Duplicate key \"" + map->get_duplicate_key()->perform(&to_string) + "\" in map " + map->perform(&to_string) + ".", pstate);
    // }

    ParserState ps = map->pstate();
    ps.offset = pstate - ps + pstate.offset;
    map->pstate(ps);

    return map;
  }

  // parse list returns either a space separated list,
  // a comma separated list or any bare expression found.
  // so to speak: we unwrap items from lists if possible here!
  Expression* Parser::parse_list()
  {
    // parse list is relly just an alias
    return parse_comma_list();
  }

  // will return singletons unwrapped
  Expression* Parser::parse_comma_list()
  {
    // check if we have an empty list
    // return the empty list as such
    if (peek_css< alternatives <
          // exactly<'!'>,
          exactly<';'>,
          exactly<'{'>,
          exactly<'}'>,
          exactly<')'>,
          exactly<':'>,
          exactly<ellipsis>,
          default_flag,
          global_flag
        > >(position))
    { return new (mem) List(pstate, 0); }

    // now try to parse a space list
    Expression* list = parse_space_list();
    // if it's a singleton, return it (don't wrap it)
    if (!peek_css< exactly<','> >(position)) return list;

    // if we got so far, we actually do have a comma list
    List* comma_list = new (mem) List(pstate, 2, List::COMMA);
    // wrap the first expression
    (*comma_list) << list;

    while (lex_css< exactly<','> >())
    {
      // check for abort condition
      if (peek_css< alternatives <
            exactly<';'>,
            exactly<'{'>,
            exactly<'}'>,
            exactly<':'>,
            exactly<')'>,
            exactly<ellipsis>,
            default_flag,
            global_flag
          > >(position)
      ) { break; }
      // otherwise add another expression
      (*comma_list) << parse_space_list();
    }
    // return the list
    return comma_list;
  }
  // EO parse_comma_list

  // will return singletons unwrapped
  Expression* Parser::parse_space_list()
  {
    Expression* disj1 = parse_disjunction();
    // if it's a singleton, return it (don't wrap it)
    if (peek_css< alternatives <
          // exactly<'!'>,
          exactly<';'>,
          exactly<'}'>,
          exactly<'{'>,
          exactly<')'>,
          exactly<':'>,
          exactly<','>,
          exactly<ellipsis>,
          default_flag,
          global_flag
        > >(position)
    ) { return disj1; }

    List* space_list = new (mem) List(pstate, 2, List::SPACE);
    (*space_list) << disj1;

    while (!(peek_css< alternatives <
               // exactly<'!'>,
               exactly<';'>,
               exactly<'}'>,
               exactly<'{'>,
               exactly<')'>,
               exactly<':'>,
               exactly<','>,
               exactly<ellipsis>,
               default_flag,
               global_flag
           > >(position)) && peek_css< optional_css_whitespace >() != end
    ) {
      // the space is parsed implicitly?
      (*space_list) << parse_disjunction();
    }
    // return the list
    return space_list;
  }
  // EO parse_space_list

  // parse logical OR operation
  Expression* Parser::parse_disjunction()
  {
    // parse the left hand side conjunction
    Expression* conj = parse_conjunction();
    // parse multiple right hand sides
    vector<Expression*> operands;
    while (lex_css< kwd_or >())
      operands.push_back(parse_conjunction());
    // if it's a singleton, return it directly
    if (operands.size() == 0) return conj;
    // fold all operands into one binary expression
    return fold_operands(conj, operands, Binary_Expression::OR);
  }
  // EO parse_disjunction

  // parse logical AND operation
  Expression* Parser::parse_conjunction()
  {
    // parse the left hand side relation
    Expression* rel = parse_relation();
    // parse multiple right hand sides
    vector<Expression*> operands;
    while (lex_css< kwd_and >())
      operands.push_back(parse_relation());
    // if it's a singleton, return it directly
    if (operands.size() == 0) return rel;
    // fold all operands into one binary expression
    return fold_operands(rel, operands, Binary_Expression::AND);
  }
  // EO parse_conjunction


  // parse comparison operations
  Expression* Parser::parse_relation()
  {
    // parse the left hand side expression
    Expression* lhs = parse_expression();
    // if it's a singleton, return it (don't wrap it)
    if (!(peek< alternatives <
            kwd_eq,
            kwd_neq,
            kwd_gte,
            kwd_gt,
            kwd_lte,
            kwd_lt
          > >(position)))
    { return lhs; }
    // parse the operator
    Binary_Expression::Type op
    = lex<kwd_eq>()  ? Binary_Expression::EQ
    : lex<kwd_neq>() ? Binary_Expression::NEQ
    : lex<kwd_gte>() ? Binary_Expression::GTE
    : lex<kwd_lte>() ? Binary_Expression::LTE
    : lex<kwd_gt>()  ? Binary_Expression::GT
    : lex<kwd_lt>()  ? Binary_Expression::LT
    // we checked the possibilites on top of fn
    :                  Binary_Expression::EQ;
    // parse the right hand side expression
    Expression* rhs = parse_expression();
    // return binary expression with a left and a right hand side
    return new (mem) Binary_Expression(lhs->pstate(), op, lhs, rhs);
  }
  // parse_relation

  // parse expression valid for operations
  // called from parse_relation
  // called from parse_for_directive
  // called from parse_media_expression
  // parse addition and subtraction operations
  Expression* Parser::parse_expression()
  {
    Expression* lhs = parse_operators();
    // if it's a singleton, return it (don't wrap it)
    if (!(peek< exactly<'+'> >(position) ||
          // condition is a bit misterious, but some combinations should not be counted as operations
          (peek< no_spaces >(position) && peek< sequence< negate< unsigned_number >, exactly<'-'>, negate< space > > >(position)) ||
          (peek< sequence< negate< unsigned_number >, exactly<'-'>, negate< unsigned_number > > >(position))) ||
          peek< identifier >(position))
    { return lhs; }

    vector<Expression*> operands;
    vector<Binary_Expression::Type> operators;
    while (lex< exactly<'+'> >() || lex< sequence< negate< digit >, exactly<'-'> > >()) {
      operators.push_back(lexed.to_string() == "+" ? Binary_Expression::ADD : Binary_Expression::SUB);
      operands.push_back(parse_operators());
    }

if (operands.size() == 0) return lhs;

    return fold_operands(lhs, operands, operators);
  }

  // parse addition and subtraction operations
  Expression* Parser::parse_operators()
  {
    Expression* factor = parse_factor();
    // Special case: Ruby sass never tries to modulo if lhs contains an interpolant
    if (peek_css< exactly<'%'> >() && factor->concrete_type() == Expression::STRING) {
      String_Schema* ss = dynamic_cast<String_Schema*>(factor);
      if (ss && ss->has_interpolants()) return factor;
    }
    // if it's a singleton, return it (don't wrap it)
    if (!peek_css< class_char< static_ops > >()) return factor;
    // parse more factors and operators
    vector<Expression*> operands; // factors
    vector<Binary_Expression::Type> operators; // ops
    // lex operations to apply to lhs
    while (lex_css< class_char< static_ops > >()) {
      switch(*lexed.begin) {
        case '*': operators.push_back(Binary_Expression::MUL); break;
        case '/': operators.push_back(Binary_Expression::DIV); break;
        case '%': operators.push_back(Binary_Expression::MOD); break;
        default: throw runtime_error("unknown static op parsed"); break;
      }
      operands.push_back(parse_factor());
    }
    // if it's a singleton, return it (don't wrap it)
    // ToDo: this check doesn't seem to work correct
    // if (operands.size() == 0) return factor;
    // operands and operators to binary expression
    return fold_operands(factor, operands, operators);
  }
  // EO parse_operators


  // called from parse_operators
  // called from parse_value_schema
  Expression* Parser::parse_factor()
  {
    if (lex_css< exactly<'('> >()) {
      // parse_map may return a list
      Expression* value = parse_map();
      // lex the expected closing parenthesis
      if (!lex_css< exactly<')'> >()) error("unclosed parenthesis", pstate);
      // expression can be evaluated
      value->is_delayed(false);
      // make sure wrapped lists and division expressions are non-delayed within parentheses
      if (value->concrete_type() == Expression::LIST) {
        List* l = static_cast<List*>(value);
        if (!l->empty()) (*l)[0]->is_delayed(false);
      } else if (typeid(*value) == typeid(Binary_Expression)) {
        Binary_Expression* b = static_cast<Binary_Expression*>(value);
        Binary_Expression* lhs = static_cast<Binary_Expression*>(b->left());
        if (lhs && lhs->type() == Binary_Expression::DIV) lhs->is_delayed(false);
      }
      return value;
    }
    else if (peek< ie_property >()) {
      return parse_ie_property();
    }
    else if (peek< ie_keyword_arg >()) {
      return parse_ie_keyword_arg();
    }
    else if (peek< exactly< calc_kwd > >() ||
             peek< exactly< moz_calc_kwd > >() ||
             peek< exactly< ms_calc_kwd > >() ||
             peek< exactly< webkit_calc_kwd > >()) {
      return parse_calc_function();
    }
    else if (peek< functional_schema >()) {
      return parse_function_call_schema();
    }
    else if (lex< identifier_schema >()) {
      return parsing_identifier_schema();
    }
    else if (peek< re_pseudo_selector >()) {
      return parse_function_call();
    }
    else if (lex< exactly<'+'> >()) {
      return new (mem) Unary_Expression(pstate, Unary_Expression::PLUS, parse_factor());
    }
    else if (lex< exactly<'-'> >()) {
      return new (mem) Unary_Expression(pstate, Unary_Expression::MINUS, parse_factor());
    }
    else if (lex< sequence< kwd_not > >()) {
      return new (mem) Unary_Expression(pstate, Unary_Expression::NOT, parse_factor());
    }
    else if (peek < sequence < one_plus < alternatives < css_whitespace, exactly<'-'>, exactly<'+'> > >, number > >()) {
      if (parse_number_prefix()) return parse_value(); // prefix is positive
      return new (mem) Unary_Expression(pstate, Unary_Expression::MINUS, parse_value());
    }
    else {
      return parse_value();
    }
  }

  // parse one value for a list
  Expression* Parser::parse_value()
  {
    lex< css_comments >();
    if (lex< ampersand >())
    {
      return new (mem) Parent_Selector(pstate, 0, not_selector); }

    if (lex< kwd_important >())
    { return new (mem) String_Constant(pstate, "!important"); }

    const char* stop;
    if ((stop = peek< value_schema >()))
    { return parse_value_schema(stop); }

    // string may be interpolated
    if (lex< quoted_string >())
    { return parse_string(); }

    if (lex< kwd_true >())
    { return new (mem) Boolean(pstate, true); }

    if (lex< kwd_false >())
    { return new (mem) Boolean(pstate, false); }

    if (lex< kwd_null >())
    { return new (mem) Null(pstate); }


    if (lex< identifier >()) {
      return new (mem) String_Constant(pstate, lexed);
    }
    if (lex< percentage >())
    { return new (mem) Textual(pstate, Textual::PERCENTAGE, lexed); }

    // match hex number first because 0x000 looks like a number followed by an indentifier
    if (lex< alternatives< hex, hex0 > >())
    { return new (mem) Textual(pstate, Textual::HEX, lexed); }

    // also handle the 10em- foo special case
    if (lex< sequence< dimension, optional< sequence< exactly<'-'>, negate< digit > > > > >())
    { return new (mem) Textual(pstate, Textual::DIMENSION, lexed); }

    if (lex< number >())
    { return new (mem) Textual(pstate, Textual::NUMBER, lexed); }

    if (lex< variable >())
    { return new (mem) Variable(pstate, Util::normalize_underscores(lexed)); }

    // Special case handling for `%` proceeding an interpolant.
    if (lex< sequence< exactly<'%'>, optional< percentage > > >())
    { return new (mem) String_Quoted(pstate, lexed); }

    // if (peek < exactly <'\0'> >()) return 0;

    error("error reading values after " + lexed.to_string(), pstate);

    // unreachable statement
    return 0;
  }


  String_Constant* Parser::parse_static_expression()
  {
    if (peek< sequence< number, optional_spaces, exactly<'/'>, optional_spaces, number > >()) {
      return parse_static_value();
    }
    return 0;
  }

  String_Constant* Parser::parse_static_value()
  {
    lex< static_value >();
    Token str(lexed);
    --str.end;
    --position;

    String_Constant* str_node = new (mem) String_Constant(pstate, str.time_wspace());
    // str_node->is_delayed(true);
    return str_node;
  }

  String* Parser::parse_string()
  {
    Token token(lexed);
    return parse_interpolated_chunk(token);
  }


  String_Schema* Parser::parse_value_schema(const char* stop)
  {

    // initialize the string schema object to add tokens
    String_Schema* schema = new (mem) String_Schema(pstate);

    if (peek<exactly<'}'>>()) {
      css_error("Invalid CSS", " after ", ": expected expression (e.g. 1px, bold), was ");
    }

    size_t num_items = 0;
    while (position < stop)
    {

      // parse space between tokens
      if (lex< spaces >() && num_items) {
        (*schema) << new (mem) String_Constant(pstate, " ");
      }
      // lex an interpolant /#{...}/
      else if (lex< exactly < hash_lbrace > >()) {
        // Try to lex static expression first
        if (lex< re_static_expression >()) {
          (*schema) << new (mem) String_Constant(pstate, lexed);
        } else {
          (*schema) << parse_list();
        }
        // ToDo: no error check here?
        lex < exactly < rbrace > >();
      }
      /*
      else if (lex< kwd_true >())
      { (*schema) << new (mem) Boolean(pstate, true); }
      else if (lex< kwd_false >())
      { (*schema) << new (mem) Boolean(pstate, false); }
      */
      // lex some string constants
      else if (lex< alternatives < exactly<'%'>, exactly < '-' >, identifier > >()) {
        (*schema) << new (mem) String_Constant(pstate, lexed);
      }
      // lex a quoted string
      else if (lex< quoted_string >()) {
        (*schema) << new (mem) String_Quoted(pstate, lexed);
      }
      // lex (normalized) variable
      else if (lex< variable >()) {
        string name(Util::normalize_underscores(lexed));
        (*schema) << new (mem) Variable(pstate, name);
      }
      // lex percentage value
      else if (lex< percentage >()) {
        (*schema) << new (mem) Textual(pstate, Textual::PERCENTAGE, lexed);
      }
      // lex dimension value
      else if (lex< dimension >()) {
        (*schema) << new (mem) Textual(pstate, Textual::DIMENSION, lexed);
      }
      // lex number value
      else if (lex< number >()) {
        (*schema) <<  new (mem) Textual(pstate, Textual::NUMBER, lexed);
      }
      // lex hex color value
      else if (lex< hex >()) {
        (*schema) << new (mem) Textual(pstate, Textual::HEX, lexed);
      }
      // lex a value in parentheses
      else if (peek< parenthese_scope >()) {
        (*schema) << parse_factor();
      }
      else {
        return schema;
        // ToDo: should probably be changed to css_error
        error("error parsing interpolated value", pstate);
      }
      ++num_items;
    }
    return schema;
  }


  // this parses interpolation outside other strings
  // means the result must not be quoted again later
  String* Parser::parse_identifier_schema()
  {
    // first lex away whatever we have found
    lex< sequence< optional< exactly<'*'> >, identifier_schema > >();
    return parsing_identifier_schema();
  }

  String* Parser::parsing_identifier_schema()
  {
    Token id(lexed);
    const char* i = id.begin;
    // see if there any interpolants
    const char* p = find_first_in_interval< exactly<hash_lbrace> >(id.begin, id.end);
    if (!p) {
      return new (mem) String_Quoted(pstate, string(id.begin, id.end));
    }

    String_Schema* schema = new (mem) String_Schema(pstate);
    while (i < id.end) {
      p = find_first_in_interval< exactly<hash_lbrace> >(i, id.end);
      if (p) {
        if (i < p) {
          // accumulate the preceding segment if it's nonempty
          (*schema) << new (mem) String_Constant(pstate, string(i, p));
        }
        // we need to skip anything inside strings
        // create a new target in parser/prelexer
        if (peek < sequence < optional_spaces, exactly<rbrace> > >(p+2)) { position = p+2;
          css_error("Invalid CSS", " after ", ": expected expression (e.g. 1px, bold), was ");
        }
        const char* j = skip_over_scopes< exactly<hash_lbrace>, exactly<rbrace> >(p+2, id.end); // find the closing brace
        if (j) {
          // parse the interpolant and accumulate it
          Expression* interp_node = Parser::from_token(Token(p+2, j), ctx, mem, pstate).parse_list();
          interp_node->is_interpolant(true);
          (*schema) << interp_node;
          schema->has_interpolants(true);
          i = j;
        }
        else {
          // throw an error if the interpolant is unterminated
          error("unterminated interpolant inside interpolated identifier " + id.to_string(), pstate);
        }
      }
      else { // no interpolants left; add the last segment if nonempty
        if (i < end) (*schema) << new (mem) String_Quoted(pstate, string(i, id.end));
        break;
      }
    }
    return schema;
  }

  // look ahead for a token with interpolation in it
  // we mostly use the result if there is an interpolation
  // everything that passes here gets parsed as one schema
  // meaning it will not be parsed as a space separated list
  Lookahead Parser::lookahead_for_value(const char* start)
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
            // consume whitespace
            block_comment, spaces,
            // main tokens
            interpolant,
            identifier,
            variable,
            // issue #442
            sequence <
              parenthese_scope,
              interpolant
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
      else if (peek < exactly<';'> >(q)) rv.found = q;
      else if (peek < exactly<'}'> >(q)) rv.found = q;
    }

    // return result
    return rv;

  }
  // EO lookahead_for_value

}
