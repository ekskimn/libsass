#ifdef _WIN32
#define PATH_SEP ';'
#else
#define PATH_SEP ':'
#endif

#include <string>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <sstream>
#include <iostream>

#include "ast.hpp"
#include "util.hpp"
#include "sass.h"
#include "context.hpp"
#include "plugins.hpp"
#include "constants.hpp"
#include "parser.hpp"
#include "file.hpp"
#include "inspect.hpp"
#include "output.hpp"
#include "expand.hpp"
#include "eval.hpp"
#include "cssize.hpp"
#include "listize.hpp"
#include "extend.hpp"
#include "remove_placeholders.hpp"
#include "color_names.hpp"
#include "functions.hpp"
#include "backtrace.hpp"
#include "sass2scss.h"
#include "prelexer.hpp"
#include "emitter.hpp"
#include "debugger.hpp"

namespace Sass {
  using namespace Constants;
  using namespace File;
  using namespace Sass;
  using std::cerr;
  using std::endl;

  Sass_Queued::Sass_Queued(const string& load_path, const string& abs_path, const char* source)
  {
    this->load_path = load_path;
    this->abs_path = abs_path;
    this->source = source;
  }

  inline bool sort_importers (const Sass_Importer_Entry& i, const Sass_Importer_Entry& j)
  { return sass_importer_get_priority(i) > sass_importer_get_priority(j); }

  Context::Context(Context::Data initializers)
  : // Output(this),
    head_imports(0),
    mem(Memory_Manager<AST_Node>()),
    c_options               (initializers.c_options()),
    c_compiler              (initializers.c_compiler()),
    source_c_str            (initializers.source_c_str()),
    sources                 (vector<const char*>()),
    plugin_paths            (initializers.plugin_paths()),
    include_paths           (initializers.include_paths()),
    queue                   (vector<Sass_Queued>()),
    style_sheets            (map<string, Block*>()),
    emitter (this),
    c_headers               (vector<Sass_Importer_Entry>()),
    c_importers             (vector<Sass_Importer_Entry>()),
    c_functions             (vector<Sass_Function_Entry>()),
    indent                  (initializers.indent()),
    linefeed                (initializers.linefeed()),
    input_path              (make_canonical_path(initializers.input_path())),
    output_path             (make_canonical_path(initializers.output_path())),
    source_comments         (initializers.source_comments()),
    output_style            (initializers.output_style()),
    source_map_file         (make_canonical_path(initializers.source_map_file())),
    source_map_root         (initializers.source_map_root()), // pass-through
    source_map_embed        (initializers.source_map_embed()),
    source_map_contents     (initializers.source_map_contents()),
    omit_source_map_url     (initializers.omit_source_map_url()),
    is_indented_syntax_src  (initializers.is_indented_syntax_src()),
    names_to_colors         (map<string, Color*>()),
    colors_to_names         (map<int, string>()),
    precision               (initializers.precision()),
    plugins(),
    subset_map              (Subset_Map<string, pair<Complex_Selector*, Compound_Selector*> >())
  {

    cwd = get_cwd();

    // enforce some safe defaults
    // used to create relative file links
    if (input_path == "") input_path = "stdin";
    if (output_path == "") output_path = "stdout";

    include_paths.push_back(cwd);
    collect_include_paths(initializers.include_paths_c_str());
    // collect_include_paths(initializers.include_paths_array());
    collect_plugin_paths(initializers.plugin_paths_c_str());
    // collect_plugin_paths(initializers.plugin_paths_array());

    setup_color_map();

    for (size_t i = 0, S = plugin_paths.size(); i < S; ++i) {
      plugins.load_plugins(plugin_paths[i]);
    }

    for(auto fn : plugins.get_functions()) {
      c_functions.push_back(fn);
    }
    for(auto fn : plugins.get_headers()) {
      c_headers.push_back(fn);
    }
    for(auto fn : plugins.get_importers()) {
      c_importers.push_back(fn);
    }

    sort (c_headers.begin(), c_headers.end(), sort_importers);
    sort (c_importers.begin(), c_importers.end(), sort_importers);
    string entry_point = initializers.entry_point();
    if (!entry_point.empty()) {
      string result(add_file(entry_point));
      if (result.empty()) {
        throw "File to read not found or unreadable: " + entry_point;
      }
    }

    emitter.set_filename(resolve_relative_path(output_path, source_map_file, cwd));

  }

  void Context::add_c_function(Sass_Function_Entry function)
  {
    c_functions.push_back(function);
  }
  void Context::add_c_header(Sass_Importer_Entry header)
  {
    c_headers.push_back(header);
    // need to sort the array afterwards (no big deal)
    sort (c_headers.begin(), c_headers.end(), sort_importers);
  }
  void Context::add_c_importer(Sass_Importer_Entry importer)
  {
    c_importers.push_back(importer);
    // need to sort the array afterwards (no big deal)
    sort (c_importers.begin(), c_importers.end(), sort_importers);
  }

  Context::~Context()
  {
    // everything that gets put into sources will be freed by us
    for (size_t n = 0; n < import_stack.size(); ++n) sass_delete_import(import_stack[n]);
    // sources are allocated by strdup or malloc (overtaken from C code)
    for (size_t i = 0; i < sources.size(); ++i) free((void*)sources[i]);
    // clear inner structures (vectors)
    sources.clear(); import_stack.clear();
  }

  void Context::setup_color_map()
  {
    size_t i = 0;
    while (color_names[i]) {
      string name(color_names[i]);
      Color* value = new (mem) Color(ParserState("[COLOR TABLE]"),
                                     color_values[i*4],
                                     color_values[i*4+1],
                                     color_values[i*4+2],
                                     color_values[i*4+3]);
      names_to_colors[name] = value;
      // only map fully opaque colors
      if (color_values[i*4+3] >= 1) {
        int numval = static_cast<int>(color_values[i*4])*0x10000;
        numval += static_cast<int>(color_values[i*4+1])*0x100;
        numval += static_cast<int>(color_values[i*4+2]);
        colors_to_names[numval] = name;
      }
      ++i;
    }
  }

  void Context::collect_include_paths(const char* paths_str)
  {

    if (paths_str) {
      const char* beg = paths_str;
      const char* end = Prelexer::find_first<PATH_SEP>(beg);

      while (end) {
        string path(beg, end - beg);
        if (!path.empty()) {
          if (*path.rbegin() != '/') path += '/';
          include_paths.push_back(path);
        }
        beg = end + 1;
        end = Prelexer::find_first<PATH_SEP>(beg);
      }

      string path(beg);
      if (!path.empty()) {
        if (*path.rbegin() != '/') path += '/';
        include_paths.push_back(path);
      }
    }
  }

  void Context::collect_include_paths(const char** paths_array)
  {
    if (paths_array) {
      for (size_t i = 0; paths_array[i]; i++) {
        collect_include_paths(paths_array[i]);
      }
    }
  }

  void Context::collect_plugin_paths(const char* paths_str)
  {

    if (paths_str) {
      const char* beg = paths_str;
      const char* end = Prelexer::find_first<PATH_SEP>(beg);

      while (end) {
        string path(beg, end - beg);
        if (!path.empty()) {
          if (*path.rbegin() != '/') path += '/';
          plugin_paths.push_back(path);
        }
        beg = end + 1;
        end = Prelexer::find_first<PATH_SEP>(beg);
      }

      string path(beg);
      if (!path.empty()) {
        if (*path.rbegin() != '/') path += '/';
        plugin_paths.push_back(path);
      }
    }
  }

  void Context::collect_plugin_paths(const char** paths_array)
  {
    if (paths_array) {
      for (size_t i = 0; paths_array[i]; i++) {
        collect_plugin_paths(paths_array[i]);
      }
    }
  }
  void Context::add_source(string load_path, string abs_path, const char* contents)
  {
    sources.push_back(contents);
    included_files.push_back(abs_path);
    queue.push_back(Sass_Queued(load_path, abs_path, contents));
    emitter.add_source_index(sources.size() - 1);
    include_links.push_back(resolve_relative_path(abs_path, source_map_file, cwd));
  }

  // Add a new import file to the context
  string Context::add_file(const string& file)
  {
    using namespace File;
    string path(make_canonical_path(file));
    string resolved(find_file(path, include_paths));
    if (resolved == "") return resolved;
    if (char* contents = read_file(resolved)) {
      add_source(path, resolved, contents);
      style_sheets[path] = 0;
      return path;
    }
    return string("");
  }

  // Add a new import file to the context
  // This has some previous directory context
  string Context::add_file(const string& base, const string& file)
  {
    using namespace File;
    string path(make_canonical_path(file));
    string base_file(join_paths(base, path));
    string resolved(resolve_file(base_file));
    if (style_sheets.count(base_file)) return base_file;
    if (char* contents = read_file(resolved)) {
      add_source(base_file, resolved, contents);
      style_sheets[base_file] = 0;
      return base_file;
    }
    // now go the regular code path
    return add_file(path);
  }

  void register_function(Context&, Memory_Manager<AST_Node>& mem, Signature sig, Native_Function f, Env* env);
  void register_function(Context&, Memory_Manager<AST_Node>& mem, Signature sig, Native_Function f, size_t arity, Env* env);
  void register_overload_stub(Context&, Memory_Manager<AST_Node>& mem, string name, Env* env);
  void register_built_in_functions(Context&, Memory_Manager<AST_Node>& mem, Env* env);
  void register_c_functions(Context&, Env* env, Sass_Function_List);
  void register_c_function(Context&, Env* env, Sass_Function_Entry);

  char* Context::compile_block(Block* root)
  {
    if (!root) return 0;
    root->perform(&emitter);
    emitter.finalize();
    OutputBuffer emitted = emitter.get_buffer();
    string output = emitted.buffer;
    if (source_map_file != "" && !omit_source_map_url) {
      output += linefeed + format_source_mapping_url(source_map_file);
    }
    return sass_strdup(output.c_str());
  }

  Block* Context::parse_file()
  {

    Block* root = 0;
    for (size_t i = 0; i < queue.size(); ++i) {
      Sass_Import_Entry import = sass_make_import(
        queue[i].load_path.c_str(),
        queue[i].abs_path.c_str(),
        0, 0
      );
      import_stack.push_back(import);
      const char* path = sass_strdup(queue[i].abs_path.c_str());
      Parser p(Parser::from_c_str(queue[i].source, *this, mem, ParserState(path, queue[i].source, i)));
      Block* ast = p.parse();
      sass_delete_import(import_stack.back());
      import_stack.pop_back();
      if (i == 0) root = ast;
      // ToDo: we store by load_path, which can lead
      // to duplicates if importer reports the same path
      // Maybe we should add an error for duplicates!?
      style_sheets[queue[i].load_path] = ast;
    }
    if (root == 0) return 0;

    Env global; // create root environment
    // register built-in functions on env
    register_built_in_functions(*this, mem, &global);
    // register custom functions (defined via C-API)
    for (size_t i = 0, S = c_functions.size(); i < S; ++i)
    { register_c_function(*this, &global, c_functions[i]); }
    // create initial backtrace entry
    Backtrace backtrace(0, ParserState("", 0), "");
    // create crtp visitor objects
    Expand expand(*this, &global, &backtrace);
    Cssize cssize(*this, &global, &backtrace);
    // expand and eval the tree
//    debug_ast(root);
    root = root->perform(&expand)->block();
//    debug_ast(root);
    // merge and bubble certain rules
    root = root->perform(&cssize)->block();
    // should we extend something?
    if (!subset_map.empty()) {
      // create crtp visitor object
      Extend extend(*this, subset_map);
      // extend tree nodes
      root->perform(&extend);
    }

    // clean up by removing empty placeholders
    // ToDo: maybe we can do this somewhere else?
    Remove_Placeholders remove_placeholders(*this);
    root->perform(&remove_placeholders);
//    debug_ast(root);

    // return processed tree
    return root;

  }
  // EO parse_file

  Block* Context::parse_string()
  {
    if (!source_c_str) return 0;
    queue.clear();
    if(is_indented_syntax_src) {
      char * contents = sass2scss(source_c_str, SASS2SCSS_PRETTIFY_1 | SASS2SCSS_KEEP_COMMENT);
      add_source(input_path, input_path, contents);
      delete [] source_c_str;
      return parse_file();
    }
    add_source(input_path, input_path, source_c_str);
    return parse_file();
  }

  char* Context::compile_file()
  {
    // returns NULL if something fails
    return compile_block(parse_file());
  }

  char* Context::compile_string()
  {
    // returns NULL if something fails
    return compile_block(parse_string());
  }

  string Context::format_source_mapping_url(const string& file)
  {
    string url = resolve_relative_path(file, output_path, cwd);
    if (source_map_embed) {
      string map = emitter.generate_source_map(*this);
      istringstream is( map );
      ostringstream buffer;
      base64::encoder E;
      E.encode(is, buffer);
      url = "data:application/json;base64," + buffer.str();
      url.erase(url.size() - 1);
    }
    return "/*# sourceMappingURL=" + url + " */";
  }

  char* Context::generate_source_map()
  {
    if (source_map_file == "") return 0;
    char* result = 0;
    string map = emitter.generate_source_map(*this);
    result = sass_strdup(map.c_str());
    return result;
  }


  std::vector<std::string> Context::get_included_files(size_t skip)
  {
      vector<string> includes = included_files;
      if (includes.size() == 0) return includes;
      std::sort( includes.begin() + skip, includes.end() );
      includes.erase( includes.begin(), includes.begin() + skip );
      includes.erase( std::unique( includes.begin(), includes.end() ), includes.end() );
      // the skip solution seems more robust, as we may have real files named stdin
      // includes.erase( std::remove( includes.begin(), includes.end(), "stdin" ), includes.end() );
      return includes;
  }

  string Context::get_cwd()
  {
    return Sass::File::get_cwd();
  }

  void register_function(Context& ctx, Memory_Manager<AST_Node>& mem, Signature sig, Native_Function f, Env* env)
  {
    Definition* def = make_native_function(sig, f, ctx);
    def->environment(env);
    (*env)[def->name() + "[f]"] = def;
  }

  void register_function(Context& ctx, Memory_Manager<AST_Node>& mem, Signature sig, Native_Function f, size_t arity, Env* env)
  {
    Definition* def = make_native_function(sig, f, ctx);
    stringstream ss;
    ss << def->name() << "[f]" << arity;
    def->environment(env);
    (*env)[ss.str()] = def;
  }

  static vector<Definition*>* defs;

  void register_overload_stub(Context& ctx, Memory_Manager<AST_Node>& mem, string name, Env* env)
  {
    Definition* stub = new (mem) Definition(ParserState("[built-in function]"),
                                            0,
                                            name,
                                            0,
                                            0,
                                            // &ctx,
                                            true);
    (*env)[name + "[f]"] = stub;
  }


  void register_built_in_functions(Context& ctx, Memory_Manager<AST_Node>& mem, Env* env)
  {
    using namespace Functions;
    // RGB Functions
    register_function(ctx, mem, rgb_sig, rgb, env);
    register_overload_stub(ctx, mem, "rgba", env);
    register_function(ctx, mem, rgba_4_sig, rgba_4, 4, env);
    register_function(ctx, mem, rgba_2_sig, rgba_2, 2, env);
    register_function(ctx, mem, red_sig, red, env);
    register_function(ctx, mem, green_sig, green, env);
    register_function(ctx, mem, blue_sig, blue, env);
    register_function(ctx, mem, mix_sig, mix, env);
    // HSL Functions
    register_function(ctx, mem, hsl_sig, hsl, env);
    register_function(ctx, mem, hsla_sig, hsla, env);
    register_function(ctx, mem, hue_sig, hue, env);
    register_function(ctx, mem, saturation_sig, saturation, env);
    register_function(ctx, mem, lightness_sig, lightness, env);
    register_function(ctx, mem, adjust_hue_sig, adjust_hue, env);
    register_function(ctx, mem, lighten_sig, lighten, env);
    register_function(ctx, mem, darken_sig, darken, env);
    register_function(ctx, mem, saturate_sig, saturate, env);
    register_function(ctx, mem, desaturate_sig, desaturate, env);
    register_function(ctx, mem, grayscale_sig, grayscale, env);
    register_function(ctx, mem, complement_sig, complement, env);
    register_function(ctx, mem, invert_sig, invert, env);
    // Opacity Functions
    register_function(ctx, mem, alpha_sig, alpha, env);
    register_function(ctx, mem, opacity_sig, alpha, env);
    register_function(ctx, mem, opacify_sig, opacify, env);
    register_function(ctx, mem, fade_in_sig, opacify, env);
    register_function(ctx, mem, transparentize_sig, transparentize, env);
    register_function(ctx, mem, fade_out_sig, transparentize, env);
    // Other Color Functions
    register_function(ctx, mem, adjust_color_sig, adjust_color, env);
    register_function(ctx, mem, scale_color_sig, scale_color, env);
    register_function(ctx, mem, change_color_sig, change_color, env);
    register_function(ctx, mem, ie_hex_str_sig, ie_hex_str, env);
    // String Functions
    register_function(ctx, mem, unquote_sig, sass_unquote, env);
    register_function(ctx, mem, quote_sig, sass_quote, env);
    register_function(ctx, mem, str_length_sig, str_length, env);
    register_function(ctx, mem, str_insert_sig, str_insert, env);
    register_function(ctx, mem, str_index_sig, str_index, env);
    register_function(ctx, mem, str_slice_sig, str_slice, env);
    register_function(ctx, mem, to_upper_case_sig, to_upper_case, env);
    register_function(ctx, mem, to_lower_case_sig, to_lower_case, env);
    // Number Functions
    register_function(ctx, mem, percentage_sig, percentage, env);
    register_function(ctx, mem, round_sig, round, env);
    register_function(ctx, mem, ceil_sig, ceil, env);
    register_function(ctx, mem, floor_sig, floor, env);
    register_function(ctx, mem, abs_sig, abs, env);
    register_function(ctx, mem, min_sig, min, env);
    register_function(ctx, mem, max_sig, max, env);
    register_function(ctx, mem, random_sig, random, env);
    // List Functions
    register_function(ctx, mem, length_sig, length, env);
    register_function(ctx, mem, nth_sig, nth, env);
    register_function(ctx, mem, set_nth_sig, set_nth, env);
    register_function(ctx, mem, index_sig, index, env);
    register_function(ctx, mem, join_sig, join, env);
    register_function(ctx, mem, append_sig, append, env);
    register_function(ctx, mem, zip_sig, zip, env);
    register_function(ctx, mem, list_separator_sig, list_separator, env);
    // Map Functions
    register_function(ctx, mem, map_get_sig, map_get, env);
    register_function(ctx, mem, map_merge_sig, map_merge, env);
    register_function(ctx, mem, map_remove_sig, map_remove, env);
    register_function(ctx, mem, map_keys_sig, map_keys, env);
    register_function(ctx, mem, map_values_sig, map_values, env);
    register_function(ctx, mem, map_has_key_sig, map_has_key, env);
    register_function(ctx, mem, keywords_sig, keywords, env);
    // Introspection Functions
    register_function(ctx, mem, type_of_sig, type_of, env);
    register_function(ctx, mem, unit_sig, unit, env);
    register_function(ctx, mem, unitless_sig, unitless, env);
    register_function(ctx, mem, comparable_sig, comparable, env);
    register_function(ctx, mem, variable_exists_sig, variable_exists, env);
    register_function(ctx, mem, global_variable_exists_sig, global_variable_exists, env);
    register_function(ctx, mem, function_exists_sig, function_exists, env);
    register_function(ctx, mem, mixin_exists_sig, mixin_exists, env);
    register_function(ctx, mem, feature_exists_sig, feature_exists, env);
    register_function(ctx, mem, call_sig, call, env);
    // Boolean Functions
    register_function(ctx, mem, not_sig, sass_not, env);
    register_function(ctx, mem, if_sig, sass_if, env);
    // Misc Functions
    register_function(ctx, mem, inspect_sig, inspect, env);
    register_function(ctx, mem, unique_id_sig, unique_id, env);
    // Selector functions
    register_function(ctx, mem, is_superselector_sig, is_superselector, env);
  }

  void register_c_functions(Context& ctx, Env* env, Sass_Function_List descrs)
  {
    while (descrs && *descrs) {
      register_c_function(ctx, env, *descrs);
      ++descrs;
    }
  }
  void register_c_function(Context& ctx, Env* env, Sass_Function_Entry descr)
  {
    Definition* def = make_c_function(descr, ctx);
    def->environment(env);
    (*env)[def->name() + "[f]"] = def;
  }


}
