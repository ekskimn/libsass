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



  Block* Parser::parse()
  {
    Block* root = new (mem) Block(pstate, 0, true);
    block_stack.push_back(root);
    read_bom();

    if (ctx.queue.size() == 1) {
      Import* pre = new (mem) Import(pstate);
      string load_path(ctx.queue[0].load_path);
      do_import(load_path, pre, ctx.c_headers, false);
      ctx.head_imports = ctx.queue.size() - 1;
      if (!pre->urls().empty()) (*root) << pre;
      if (!pre->files().empty()) {
        for (size_t i = 0, S = pre->files().size(); i < S; ++i) {
          (*root) << new (mem) Import_Stub(pstate, pre->files()[i]);
        }
      }
    }

    parse_block(root);
    return root;

  }

  // convenience function for block parsing
  // will create a new block ad-hoc for you
  Block* Parser::parse_block(bool chroot)
  {
    // create new block and pass it to actual parse function
    return parse_block(new (mem) Block(pstate, 0, chroot));
  }

  // the main block parsing function
  // parses stuff between `{` and `}`
  Block* Parser::parse_block(Block* root)
  {
    // block_stack.push_back(root);
    // loop until end of string
    while (position < end) {
      // parse comment blocks
      parse_block_comments(root);
      // check scope closing condition
      if (lex< exactly<'}'> >()) break;
      // delegate rest to the block parser
      else if (parse_block_nodes(root)) continue;
    }
    // block_stack.pop_back();
    // return passed pointer
    // used for syntax sugar
    return root;
  }

  // main parser for anything between `{` and `}`
  bool Parser::parse_block_nodes(Block* block) {

    // throw away white-space
    // includes line comments
    lex < css_whitespace >();

    Lookahead lookahead_result;

    if (!block->is_root() && lex< kwd_extend >(true)) {
      Lookahead lookahead = lookahead_for_include(position);
      if (!lookahead.found) error("invalid selector for @extend", pstate);
      Selector* target;
      if (lookahead.has_interpolants) target = parse_selector_schema(lookahead.found);
      else                            target = parse_selector_list();
      (*block) << new (mem) Extension(pstate, target);
    }


    else if (lex< kwd_import >(true)) {
      if (stack.back() == mixin_def || stack.back() == function_def) {
        error("@import directives are not allowed inside mixins and functions", pstate);
      }
      Import* imp = parse_import();
      if (!imp->urls().empty()) (*block) << imp;
      if (!imp->files().empty()) {
        for (size_t i = 0, S = imp->files().size(); i < S; ++i) {
          (*block) << new (mem) Import_Stub(pstate, imp->files()[i]);
        }
      }
    }
    // consume any superfluous semicolons
    else if (lex< one_plus< exactly<';'> > >(true)) {}

    else if (lex< kwd_content >(true)) {
      if (stack.back() == mixin_def) *block << new (mem) Content(pstate);
      else error("@content may only be used within a mixin", pstate);
    }
    else if ((lookahead_result = lookahead_for_selector(position)).found) {
      (*block) << parse_ruleset(lookahead_result);
    }
    else if (lex < kwd_media >(true)) { (*block) << parse_media_block(); }
    else if (lex < kwd_at_root >(true)) { (*block) << parse_at_root_block(); }
    else if (lex < kwd_include_directive >(true)) { (*block) << parse_include_directive(); }
    else if (lex < kwd_if_directive >(true)) { (*block) << parse_if_directive(); }
    else if (lex < kwd_for_directive >(true)) { (*block) << parse_for_directive(); }
    else if (lex < kwd_each_directive >(true)) { (*block) << parse_each_directive(); }
    else if (lex < kwd_while_directive >(true)) { (*block) << parse_while_directive(); }
    else if (lex < kwd_return_directive >(true)) { (*block) << parse_return_directive(); }
    else if (lex < kwd_supports >(true)) { (*block) << parse_feature_block(); }
    else if (lex < variable >(true)) { (*block) << parse_assignment(); }
    else if (lex < kwd_warn >(true)) { (*block) << parse_warning(); }
    else if (lex < kwd_err >(true)) { (*block) << parse_error(); }
    else if (lex < kwd_dbg >(true)) { (*block) << parse_debug(); }
    else if (lex < kwd_mixin >(true)) { (*block) << parse_definition(Definition::MIXIN); }
    else if (lex < kwd_function >(true)) { (*block) << parse_definition(Definition::FUNCTION); }

    // ignore the @charset directive for now
    else if (lex< exactly< charset_kwd > >(true)) {
      lex <
        sequence <
          quoted_string,
          optional_spaces,
          exactly<';'>
        >
      >();
    }
    // generic at keyword (keep last)
    else if (lex< at_keyword >(true)) {
      (*block) << parse_at_rule();
    }

    else if (!block->is_root() && !peek< exactly<';'> >()) {

      Declaration* decl = parse_declaration();
      decl->tabs(indentation);
      (*block) << decl;
      if (lex< exactly<'{'> >()) {
        // parse a propset that rides on the declaration's property
        if (!decl->surfer()) indentation++;
        Propset* ps = new (mem) Propset(pstate, decl->property(), parse_block());
        if (!decl->surfer()) indentation--;
        (*block) << ps;
      }
      else {
        // finish and let the semicolon get munched
      }
    }
    else if (!block->is_root() && stack.back() == function_def) {
      error("only variable declarations and control directives are allowed inside functions", pstate);
    }

    else if (block->is_root()) {
      lex< css_whitespace >();
      if (position >= end) return true;
      css_error("Invalid CSS", " after ", ": expected selector or at-rule, was ");
    }

    // nothing matched
    else { return false; }
    // something matched
    return true;
  }

  At_Root_Block* Parser::parse_at_root_block()
  {
    Lookahead lookahead_result;
    ParserState at_source_position = pstate;
    Block* body = 0;
    At_Root_Expression* expr = 0;
    in_at_root = true;
    if (lex< exactly<'('> >()) {
      expr = parse_at_root_expression();
    }
    if (lex< exactly<'{'> >()) {
      body = parse_block(true);
    }
    else if ((lookahead_result = lookahead_for_selector(position)).found) {
      Ruleset* r = parse_ruleset(lookahead_result);
      body = new (mem) Block(r->pstate(), 1, true);
      *body << r;
    }
    in_at_root = false;
    At_Root_Block* at_root = new (mem) At_Root_Block(at_source_position, body);
    if (expr) at_root->expression(expr);
    return at_root;
  }



  At_Root_Expression* Parser::parse_at_root_expression()
  {
    if (peek< exactly<')'> >()) error("at-root feature required in at-root expression", pstate);

    if (!peek< alternatives< kwd_with_directive, kwd_without_directive > >()) {
      css_error("Invalid CSS", " after ", ": expected \"with\" or \"without\", was ");
    }

    Declaration* declaration = parse_declaration();
    List* value = new (mem) List(declaration->value()->pstate(), 1);

    if (declaration->value()->concrete_type() == Expression::LIST) {
        value = static_cast<List*>(declaration->value());
    }
    else *value << declaration->value();

    At_Root_Expression* cond = new (mem) At_Root_Expression(declaration->pstate(),
                                                                declaration->property(),
                                                                value);
    if (!lex< exactly<')'> >()) error("unclosed parenthesis in @at-root expression", pstate);
    return cond;
  }

  At_Rule* Parser::parse_at_rule()
  {
    string kwd(lexed);
    ParserState at_source_position = pstate;
    Selector* sel = 0;
    Expression* val = 0;
    Lookahead lookahead = lookahead_for_include(position);
    if (lookahead.found) {
      if (lookahead.has_interpolants) {
        sel = parse_selector_schema(lookahead.found);
      }
      else {
        sel = parse_selector_list();
      }
    }
    else if (!(peek<exactly<'{'> >() || peek<exactly<'}'> >() || peek<exactly<';'> >())) {
      val = parse_list();
    }
    Block* body = 0;
    if (lex< exactly<'{'> >()) body = parse_block();
    At_Rule* rule = new (mem) At_Rule(at_source_position, kwd, sel, body);
    if (!sel) rule->value(val);
    return rule;
  }


  Declaration* Parser::parse_declaration() {
    String* prop = 0;
    not_selector = true;
    if (lex< sequence< optional< exactly<'*'> >, identifier_schema > >()) {
      prop = parsing_identifier_schema();
    }
    else if (lex< sequence< optional< exactly<'*'> >, identifier > >()) {
      prop = new (mem) String_Quoted(pstate, lexed);
    }
    else {
      error("invalid property name", pstate);
    }
    bool surfer = false;
    const string property(lexed);
    if (!lex_css< one_plus< exactly<':'> > >()) error("property \"" + property + "\" must be followed by a ':'", pstate);
    if (peek_css< exactly<';'> >()) error("style declaration must contain a value", pstate);
    if (peek_css< exactly<'{'> >()) surfer = true; // surf with the indentation
    not_selector = false;
    if (peek_css< static_value >()) {
      return new (mem) Declaration(prop->pstate(), prop, parse_static_value()/*, lex<important>()*/);
    }
    else {
      Expression* value;
      not_selector = true;
      Lookahead lookahead = lookahead_for_value(position);
      if (lookahead.found) {
        if (lookahead.has_interpolants) {
          value = parse_value_schema(lookahead.found);
        } else {
          value = parse_list();
        }
      }
      else {
        value = parse_list();
        if (List* list = dynamic_cast<List*>(value)) {
          if (list->length() == 0 && !peek< exactly <'{'> >()) {
            css_error("Invalid CSS", " after ", ": expected expression (e.g. 1px, bold), was ");
          }
        }
      }

      auto decl = new (mem) Declaration(prop->pstate(), prop, value/*, lex<important>()*/);
      decl->surfer(surfer);
      not_selector = false;
      return decl;
    }
  }

  Import* Parser::parse_import()
  {
    Import* imp = new (mem) Import(pstate);
    bool first = true;
    do {
      while (lex< block_comment >());
      if (lex< quoted_string >()) {
        if (!do_import(lexed, imp, ctx.c_importers, true))
        {
          // push single file import
          import_single_file(imp, lexed);
        }
      }
      else if (lex< uri_prefix >()) {
        Arguments* args = new (mem) Arguments(pstate);
        Function_Call* result = new (mem) Function_Call(pstate, "url", args);
        if (lex< quoted_string >()) {
          Expression* the_url = parse_string();
          *args << new (mem) Argument(the_url->pstate(), the_url);
        }
        else if (lex < uri_value >(position != 0)) { // chunk seems to work too!
          String* the_url = parse_interpolated_chunk(lexed);
          *args << new (mem) Argument(the_url->pstate(), the_url);
        }
        else if (peek < skip_over_scopes < exactly < '(' >, exactly < ')' > > >(position)) {
          Expression* the_url = parse_list(); // parse_interpolated_chunk(lexed);
          *args << new (mem) Argument(the_url->pstate(), the_url);
        }
        else {
          error("malformed URL", pstate);
        }
        if (!lex< exactly<')'> >()) error("URI is missing ')'", pstate);
        imp->urls().push_back(result);
      }
      else {
        if (first) error("@import directive requires a url or quoted path", pstate);
        else error("expecting another url or quoted path in @import list", pstate);
      }
      first = false;
    } while (lex_css< exactly<','> >());
    return imp;
  }

  Definition* Parser::parse_definition(Definition::Type which_type)
  {
    string which_str(lexed);
    if (!lex< identifier >()) error("invalid name in " + which_str + " definition", pstate);
    string name(Util::normalize_underscores(lexed));
    if (which_type == Definition::FUNCTION && (name == "and" || name == "or" || name == "not"))
    { error("Invalid function name \"" + name + "\".", pstate); }
    ParserState source_position_of_def = pstate;
    Parameters* params = parse_parameters();
    if (!lex< exactly<'{'> >()) error("body for " + which_str + " " + name + " must begin with a '{'", pstate);
    if (which_type == Definition::MIXIN) stack.push_back(mixin_def);
    else stack.push_back(function_def);
    Block* body = parse_block();
    stack.pop_back();
    Definition* def = new (mem) Definition(source_position_of_def, name, params, body, which_type);
    return def;
  }


  Ruleset* Parser::parse_ruleset(Lookahead lookahead, bool at_root)
  {
    Selector* sel;
    if (lookahead.has_interpolants) {
      sel = parse_selector_schema(lookahead.found);
    }
    else {
      sel = parse_selector_list();
    }
    bool old_in_at_root = in_at_root;
    ParserState r_source_position = pstate;
    lex < css_comments >();
    in_at_root = false;
    if (!lex< exactly<'{'> >()) error("expected a '{' after the selector", pstate);
    Block* block = parse_block();
    in_at_root = old_in_at_root;
    old_in_at_root = false;
    Ruleset* ruleset = new (mem) Ruleset(r_source_position, sel, block);
    return ruleset;
  }


  /* parse block comment and add to block */
  void Parser::parse_block_comments(Block* block)
  {
    while (lex< block_comment >()) {
      bool is_important = lexed.begin[2] == '!';
      String*  contents = parse_interpolated_chunk(lexed);
      (*block) << new (mem) Comment(pstate, contents, is_important);
    }
  }

}

