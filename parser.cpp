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

  Parser Parser::from_c_str(const char* str, Context& ctx, Memory_Manager<AST_Node>& mem, ParserState pstate)
  {
    Parser p(ctx, mem, pstate);
    p.source   = str;
    p.position = p.source;
    p.end      = str + strlen(str);
    Block* root = new (mem) Block(pstate);
    p.block_stack.push_back(root);
    root->is_root(true);
    return p;
  }

  Parser Parser::from_c_str(const char* beg, const char* end, Context& ctx, Memory_Manager<AST_Node>& mem, ParserState pstate)
  {
    Parser p(ctx, mem, pstate);
    p.source   = beg;
    p.position = p.source;
    p.end      = end;
    Block* root = new (mem) Block(pstate);
    p.block_stack.push_back(root);
    root->is_root(true);
    return p;
  }

  Selector_List* Parser::parse_selector(const char* src, Context& ctx, Memory_Manager<AST_Node>& mem, ParserState pstate)
  {
    Parser p = Parser::from_c_str(src, ctx, mem, pstate);
    // ToDo: ruby sass errors on parent references
    // ToDo: remap the source-map entries somehow
    return p.parse_selector_list();
  }

  bool Parser::peek_newline(const char* start)
  {
    return peek_linefeed(start ? start : position);
  }

  Parser Parser::from_token(Token t, Context& ctx, Memory_Manager<AST_Node>& mem, ParserState pstate)
  {
    Parser p(ctx, mem, pstate);
    p.source   = t.begin;
    p.position = p.source;
    p.end      = t.end;
    Block* root = new (mem) Block(pstate);
    p.block_stack.push_back(root);
    root->is_root(true);
    return p;
  }


  void Parser::add_single_file (Import* imp, string import_path) {

    string extension;
    string unquoted(unquote(import_path));
    if (unquoted.length() > 4) { // 2 quote marks + the 4 chars in .css
      // a string constant is guaranteed to end with a quote mark, so make sure to skip it when indexing from the end
      extension = unquoted.substr(unquoted.length() - 4, 4);
    }

    if (extension == ".css") {
      String_Constant* loc = new (mem) String_Constant(pstate, unquote(import_path));
      Argument* loc_arg = new (mem) Argument(pstate, loc);
      Arguments* loc_args = new (mem) Arguments(pstate);
      (*loc_args) << loc_arg;
      Function_Call* new_url = new (mem) Function_Call(pstate, "url", loc_args);
      imp->urls().push_back(new_url);
    }
    else {
      string current_dir = File::dir_name(path);
      string resolved(ctx.add_file(current_dir, unquoted));
      if (resolved.empty()) error("file to import not found or unreadable: " + unquoted + "\nCurrent dir: " + current_dir, pstate);
      imp->files().push_back(resolved);
    }

  }

  void Parser::import_single_file (Import* imp, string import_path) {

    if (!unquote(import_path).substr(0, 7).compare("http://") ||
        !unquote(import_path).substr(0, 8).compare("https://") ||
        !unquote(import_path).substr(0, 2).compare("//"))
    {
      imp->urls().push_back(new (mem) String_Quoted(pstate, import_path));
    }
    else {
      add_single_file(imp, import_path);
    }

  }


  bool Parser::do_import(const string& import_path, Import* imp, vector<Sass_Importer_Entry> importers, bool only_one)
  {
    bool has_import = false;
    string load_path = unquote(import_path);
    for (auto importer : importers) {
      // int priority = sass_importer_get_priority(importer);
      Sass_Importer_Fn fn = sass_importer_get_function(importer);
      if (Sass_Import_List includes =
          fn(load_path.c_str(), importer, ctx.c_compiler)
      ) {
        Sass_Import_List list = includes;
        while (*includes) {
          Sass_Import_Entry include = *includes;
          const char *file = sass_import_get_path(include);
          char* source = sass_import_take_source(include);
          size_t line = sass_import_get_error_line(include);
          size_t column = sass_import_get_error_column(include);
          const char* message = sass_import_get_error_message(include);
          if (message) {
            if (line == string::npos && column == string::npos) error(message, pstate);
            else error(message, ParserState(message, source, Position(line, column)));
          } else if (source) {
            if (file) {
              ctx.add_source(file, load_path, source);
              imp->files().push_back(file);
            } else {
              ctx.add_source(load_path, load_path, source);
              imp->files().push_back(load_path);
            }
          } else if(file) {
            import_single_file(imp, file);
          }
          ++includes;
        }
        // deallocate returned memory
        sass_delete_import_list(list);
        // set success flag
        has_import = true;
        // break import chain
        if (only_one) return true;
      }
    }
    // return result
    return has_import;
  }



  // this parses interpolation inside other strings
  // means the result should later be quoted again
  String* Parser::parse_interpolated_chunk(Token chunk, bool constant)
  {
    const char* i = chunk.begin;
    // see if there any interpolants
    const char* p = find_first_in_interval< exactly<hash_lbrace> >(i, chunk.end);
    if (!p) {
      String_Quoted* str_quoted = new (mem) String_Quoted(pstate, string(i, chunk.end));
      if (!constant && str_quoted->quote_mark()) str_quoted->quote_mark('*');
      str_quoted->is_delayed(true);
      return str_quoted;
    }

    String_Schema* schema = new (mem) String_Schema(pstate);
    while (i < chunk.end) {
      p = find_first_in_interval< exactly<hash_lbrace> >(i, chunk.end);
      if (p) {
        if (i < p) {
          // accumulate the preceding segment if it's nonempty
          (*schema) << new (mem) String_Quoted(pstate, string(i, p));
        }
        // we need to skip anything inside strings
        // create a new target in parser/prelexer
        if (peek < sequence < optional_spaces, exactly<rbrace> > >(p+2)) { position = p+2;
          css_error("Invalid CSS", " after ", ": expected expression (e.g. 1px, bold), was ");
        }
        const char* j = skip_over_scopes< exactly<hash_lbrace>, exactly<rbrace> >(p + 2, chunk.end); // find the closing brace
        if (j) { --j;
          // parse the interpolant and accumulate it
          Expression* interp_node = Parser::from_token(Token(p+2, j), ctx, mem, pstate).parse_list();
          interp_node->is_interpolant(true);
          (*schema) << interp_node;
          i = j;
        }
        else {
          // throw an error if the interpolant is unterminated
          error("unterminated interpolant inside string constant " + chunk.to_string(), pstate);
        }
      }
      else { // no interpolants left; add the last segment if nonempty
        // check if we need quotes here (was not sure after merge)
        if (i < chunk.end) (*schema) << new (mem) String_Quoted(pstate, string(i, chunk.end));
        break;
      }
      ++ i;
    }
    return schema;
  }


  String* Parser::parse_ie_property()
  {
    lex< ie_property >();
    Token str(lexed);
    const char* i = str.begin;
    // see if there any interpolants
    const char* p = find_first_in_interval< exactly<hash_lbrace> >(str.begin, str.end);
    if (!p) {
      String_Constant* str_node = new (mem) String_Constant(pstate, normalize_wspace(string(str.begin, str.end)));
      str_node->is_delayed(true);
      str_node->quote_mark('*');
      return str_node;
    }

    String_Schema* schema = new (mem) String_Schema(pstate);
    while (i < str.end) {
      p = find_first_in_interval< exactly<hash_lbrace> >(i, str.end);
      if (p) {
        if (i < p) {
          String_Constant* part = new (mem) String_Constant(pstate, normalize_wspace(string(i, p))); // accumulate the preceding segment if it's nonempty
          part->is_delayed(true);
          part->quote_mark('*'); // avoid unquote in interpolation
          (*schema) << part;
        }
        if (peek < sequence < optional_spaces, exactly<rbrace> > >(p+2)) { position = p+2;
          css_error("Invalid CSS", " after ", ": expected expression (e.g. 1px, bold), was ");
        }
        const char* j = skip_over_scopes< exactly<hash_lbrace>, exactly<rbrace> >(p+2, str.end); // find the closing brace
        if (j) {
          // parse the interpolant and accumulate it
          Expression* interp_node = Parser::from_token(Token(p+2, j), ctx, mem, pstate).parse_list();
          interp_node->is_interpolant(true);
          (*schema) << interp_node;
          i = j;
        }
        else {
          // throw an error if the interpolant is unterminated
          error("unterminated interpolant inside IE function " + str.to_string(), pstate);
        }
      }
      else { // no interpolants left; add the last segment if nonempty
        if (i < str.end) {
          String_Constant* part = new (mem) String_Constant(pstate, normalize_wspace(string(i, str.end)));
          part->is_delayed(true);
          part->quote_mark('*'); // avoid unquote in interpolation
          (*schema) << part;
        }
        break;
      }
    }
    return schema;
  }

  String* Parser::parse_ie_keyword_arg()
  {
    String_Schema* kwd_arg = new (mem) String_Schema(pstate, 3);
    if (lex< variable >()) {
      *kwd_arg << new (mem) Variable(pstate, Util::normalize_underscores(lexed));
    } else {
      lex< alternatives< identifier_schema, identifier > >();
      *kwd_arg << new (mem) String_Constant(pstate, lexed);
    }
    lex< exactly<'='> >();
    *kwd_arg << new (mem) String_Constant(pstate, lexed);
    if (peek< variable >()) *kwd_arg << parse_list();
    else if (lex< number >()) *kwd_arg << new (mem) Textual(pstate, Textual::NUMBER, Util::normalize_decimals(lexed));
    else if (peek < ie_keyword_arg_value >()) { *kwd_arg << parse_list(); }
    return kwd_arg;
  }

  void Parser::read_bom()
  {
    size_t skip = 0;
    string encoding;
    bool utf_8 = false;
    switch ((unsigned char) source[0]) {
    case 0xEF:
      skip = check_bom_chars(source, end, utf_8_bom, 3);
      encoding = "UTF-8";
      utf_8 = true;
      break;
    case 0xFE:
      skip = check_bom_chars(source, end, utf_16_bom_be, 2);
      encoding = "UTF-16 (big endian)";
      break;
    case 0xFF:
      skip = check_bom_chars(source, end, utf_16_bom_le, 2);
      skip += (skip ? check_bom_chars(source, end, utf_32_bom_le, 4) : 0);
      encoding = (skip == 2 ? "UTF-16 (little endian)" : "UTF-32 (little endian)");
      break;
    case 0x00:
      skip = check_bom_chars(source, end, utf_32_bom_be, 4);
      encoding = "UTF-32 (big endian)";
      break;
    case 0x2B:
      skip = check_bom_chars(source, end, utf_7_bom_1, 4)
           | check_bom_chars(source, end, utf_7_bom_2, 4)
           | check_bom_chars(source, end, utf_7_bom_3, 4)
           | check_bom_chars(source, end, utf_7_bom_4, 4)
           | check_bom_chars(source, end, utf_7_bom_5, 5);
      encoding = "UTF-7";
      break;
    case 0xF7:
      skip = check_bom_chars(source, end, utf_1_bom, 3);
      encoding = "UTF-1";
      break;
    case 0xDD:
      skip = check_bom_chars(source, end, utf_ebcdic_bom, 4);
      encoding = "UTF-EBCDIC";
      break;
    case 0x0E:
      skip = check_bom_chars(source, end, scsu_bom, 3);
      encoding = "SCSU";
      break;
    case 0xFB:
      skip = check_bom_chars(source, end, bocu_1_bom, 3);
      encoding = "BOCU-1";
      break;
    case 0x84:
      skip = check_bom_chars(source, end, gb_18030_bom, 4);
      encoding = "GB-18030";
      break;
    }
    if (skip > 0 && !utf_8) error("only UTF-8 documents are currently supported; your document appears to be " + encoding, pstate);
    position += skip;
  }

  size_t check_bom_chars(const char* src, const char *end, const unsigned char* bom, size_t len)
  {
    size_t skip = 0;
    if (src + len > end) return 0;
    for (size_t i = 0; i < len; ++i, ++skip) {
      if ((unsigned char) src[i] != bom[i]) return 0;
    }
    return skip;
  }


  Expression* Parser::fold_operands(Expression* base, vector<Expression*>& operands, Binary_Expression::Type op)
  {
    for (size_t i = 0, S = operands.size(); i < S; ++i) {
      base = new (mem) Binary_Expression(pstate, op, base, operands[i]);
      Binary_Expression* b = static_cast<Binary_Expression*>(base);
      if (op == Binary_Expression::DIV && b->left()->is_delayed() && b->right()->is_delayed()) {
        base->is_delayed(true);
      }
      else {
        b->left()->is_delayed(false);
        b->right()->is_delayed(false);
      }
    }
    return base;
  }

  Expression* Parser::fold_operands(Expression* base, vector<Expression*>& operands, vector<Binary_Expression::Type>& ops)
  {
    for (size_t i = 0, S = operands.size(); i < S; ++i) {
      base = new (mem) Binary_Expression(base->pstate(), ops[i], base, operands[i]);
      Binary_Expression* b = static_cast<Binary_Expression*>(base);
      if (ops[i] == Binary_Expression::DIV && b->left()->is_delayed() && b->right()->is_delayed()) {
        base->is_delayed(true);
      }
      else {
        b->left()->is_delayed(false);
        b->right()->is_delayed(false);
      }
    }
    return base;
  }

  void Parser::error(string msg, Position pos)
  {
    throw Sass_Error(Sass_Error::syntax, ParserState(path, source, pos.line ? pos : before_token, Offset(0, 0)), msg);
  }

  // print a css parsing error with actual context information from parsed source
  void Parser::css_error(const string& msg, const string& prefix, const string& middle)
  {
    int max_len = 14;
    const char* pos = peek < optional_spaces >();
    bool ellipsis_left = false;
    const char* pos_left(pos);
    while (*pos_left && pos_left > source) {
      if (pos - pos_left > max_len) {
        ellipsis_left = true;
        break;
      }
      const char* prev = pos_left - 1;
      if (*prev == '\r') break;
      if (*prev == '\n') break;
      if (*prev == 10) break;
      pos_left = prev;
    }
    bool ellipsis_right = false;
    const char* pos_right(pos);
    while (*pos_right && pos_right <= end) {
      if (pos_right - pos > max_len) {
        ellipsis_right = true;
        break;
      }
      if (*pos_right == '\r') break;
      if (*pos_right == '\n') break;
      if (*pos_left == 10) break;
      ++ pos_right;
    }
    string left(pos_left, pos);
    string right(pos, pos_right);
    if (ellipsis_left) left = ellipsis + left;
    if (ellipsis_right) right = right + ellipsis;
    // now pass new message to the more generic error function
    error(msg + prefix + quote(left) + middle + quote(right), pstate);
  }

}
