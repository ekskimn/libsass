#ifdef _MSC_VER
#pragma warning(disable : 4503)
#endif

#include <iostream>
#include <typeinfo>

#include "expand.hpp"
#include "bind.hpp"
#include "eval.hpp"
#include "to_string.hpp"
#include "backtrace.hpp"
#include "context.hpp"
#include "parser.hpp"

namespace Sass {

  Expand::Expand(Context& ctx, Env* env, Backtrace* bt)
  : ctx(ctx),
    eval(Eval(*this)),
    env_stack(vector<Env*>()),
    block_stack(vector<Block*>()),
    property_stack(vector<String*>()),
    selector_stack(vector<Selector*>()),
    backtrace_stack(vector<Backtrace*>()),
    in_at_root(false),
    in_keyframes(false)
  {
    env_stack.push_back(0);
    env_stack.push_back(env);
    block_stack.push_back(0);
    property_stack.push_back(0);
    selector_stack.push_back(0);
    backtrace_stack.push_back(0);
    backtrace_stack.push_back(bt);
  }


  Context& Expand::context()
  {
    return ctx;
  }

  Env* Expand::environment()
  {
    if (env_stack.size())
      return env_stack.back();
    return 0;
  }

  Selector* Expand::selector()
  {
    if (selector_stack.size())
      return selector_stack.back();
    return 0;
  }

  Backtrace* Expand::backtrace()
  {
    if (backtrace_stack.size())
      return backtrace_stack.back();
    return 0;
  }

  // blocks create new variable scopes
  Statement* Expand::operator()(Block* b)
  {
    // create new local environment
    // set the current env as parent
    Env env(environment());
    // copy the block object (add items later)
    Block* bb = new (ctx.mem) Block(b->pstate(),
                                    b->length(),
                                    b->is_root());
    // setup block and env stack
    this->block_stack.push_back(bb);
    this->env_stack.push_back(&env);
    // operate on block
    this->append_block(b);
    // revert block and env stack
    this->block_stack.pop_back();
    this->env_stack.pop_back();
    // return copy
    return bb;
  }

  // process and add to last block on stack
  inline void Expand::append_block(Block* b)
  {
    for (size_t i = 0, L = b->length(); i < L; ++i) {
      Statement* ith = (*b)[i]->perform(this);
      if (ith) *block_stack.back() << ith;
    }
  }

  Statement* Expand::operator()(Ruleset* r)
  {
    // automatically reset on exit
    LOCAL_FLAG(in_at_root, false);

    if (in_keyframes) {
      Keyframe_Rule* k = new (ctx.mem) Keyframe_Rule(r->pstate(), r->block()->perform(this)->block());
      if (r->selector()) {
        selector_stack.push_back(0);
        // Contextualize contextual(eval.snapshot());
        k->selector(static_cast<Selector_List*>(r->selector()->perform(eval.snapshot())));
        selector_stack.pop_back();
      }
      return k;
    }
    // debug_ast(r->selector());
    // Contextualize contextual(eval.snapshot());
    Selector* sel_ctx = static_cast<Selector*>(r->selector()->perform(eval.snapshot()));
    if (sel_ctx == 0) throw "Cannot expand null selector";

    // ToDo: Check if we can do this different
    // At least only re-parse selector schemas
    Emitter emitter(&ctx);
    Inspect isp(emitter);
    sel_ctx->perform(&isp);
    string str = isp.get_buffer();
    str += ";";

    Parser p(ctx, r->pstate());
    p.block_stack.push_back(block_stack.back());
    p.last_media_block = r->selector() ? r->selector()->media_block() : 0;
    p.source   = str.c_str();
    p.position = str.c_str();
    p.end      = str.c_str() + strlen(str.c_str());
    Selector_List* sel_lst = p.parse_selector_group();
    // sel_lst->pstate(isp.remap(sel_lst->pstate()));

    sel_ctx = sel_lst;

    selector_stack.push_back(sel_ctx);
    Block* blk = r->block()->perform(this)->block();
    Ruleset* rr = new (ctx.mem) Ruleset(r->pstate(),
                                        sel_ctx,
                                        blk);
    rr->tabs(r->tabs());
    selector_stack.pop_back();
    return rr;
  }

  Statement* Expand::operator()(Propset* p)
  {
    property_stack.push_back(p->property_fragment());
    Block* expanded_block = p->block()->perform(this)->block();

    for (size_t i = 0, L = expanded_block->length(); i < L; ++i) {
      Statement* stm = (*expanded_block)[i];
      if (Declaration* dec = static_cast<Declaration*>(stm)) {
        String_Schema* combined_prop = new (ctx.mem) String_Schema(p->pstate());
        if (!property_stack.empty()) {
          *combined_prop << property_stack.back()
                         << new (ctx.mem) String_Constant(p->pstate(), "-")
                         << dec->property(); // TODO: eval the prop into a string constant
        }
        else {
          *combined_prop << dec->property();
        }
        dec->property(combined_prop);
        *block_stack.back() << dec;
      }
      else if (typeid(*stm) == typeid(Comment)) {
        // drop comments in propsets
      }
      else {
        error("contents of namespaced properties must result in style declarations only", stm->pstate(), backtrace());
      }
    }

    property_stack.pop_back();

    return 0;
  }

  Statement* Expand::operator()(Feature_Block* f)
  {
    Expression* feature_queries = f->feature_queries()->perform(eval.snapshot());
    Feature_Block* ff = new (ctx.mem) Feature_Block(f->pstate(),
                                                    static_cast<Feature_Query*>(feature_queries),
                                                    f->block()->perform(this)->block());
    ff->selector(selector_stack.back());
    return ff;
  }

  Statement* Expand::operator()(Media_Block* m)
  {
    To_String to_string(&ctx);
    Expression* mq = m->media_queries()->perform(eval.snapshot());
    mq = Parser::from_c_str(mq->perform(&to_string).c_str(), ctx, mq->pstate()).parse_media_queries();
    Media_Block* mm = new (ctx.mem) Media_Block(m->pstate(),
                                                static_cast<List*>(mq),
                                                m->block()->perform(this)->block(),
                                                selector_stack.back());
    mm->tabs(m->tabs());
    return mm;
  }

  Statement* Expand::operator()(At_Root_Block* a)
  {
    in_at_root = true;
    Block* ab = a->block();
    Expression* ae = a->expression();
    if (ae) ae = ae->perform(eval.snapshot());
    else ae = new (ctx.mem) At_Root_Expression(a->pstate());
    Block* bb = ab ? ab->perform(this)->block() : 0;
    At_Root_Block* aa = new (ctx.mem) At_Root_Block(a->pstate(),
                                                    bb,
                                                    static_cast<At_Root_Expression*>(ae));
    in_at_root = false;
    return aa;
  }

  Statement* Expand::operator()(At_Rule* a)
  {
    LOCAL_FLAG(in_keyframes, a->is_keyframes());
    Block* ab = a->block();
    Selector* as = a->selector();
    Expression* av = a->value();
    selector_stack.push_back(0);
    // Contextualize contextual(eval.snapshot());
    if (as) as = static_cast<Selector_List*>(as->perform(eval.snapshot()));
    else if (av) av = av->perform(eval.snapshot());
    selector_stack.pop_back();
    Block* bb = ab ? ab->perform(this)->block() : 0;
    At_Rule* aa = new (ctx.mem) At_Rule(a->pstate(),
                                        a->keyword(),
                                        as,
                                        bb);
    if (av) aa->value(av);
    return aa;
  }

  Statement* Expand::operator()(Declaration* d)
  {
    String* old_p = d->property();
    String* new_p = static_cast<String*>(old_p->perform(eval.snapshot()));
    Expression* value = d->value()->perform(eval.snapshot());
    if (!value || (value->is_invisible() && !d->is_important())) return 0;
    Declaration* decl = new (ctx.mem) Declaration(d->pstate(),
                                                  new_p,
                                                  value,
                                                  d->is_important());
    decl->tabs(d->tabs());
    return decl;
  }

  Statement* Expand::operator()(Assignment* a)
  {
    Env* env = environment();
    string var(a->variable());
    if (a->is_global()) {
      if (a->is_default()) {
        if (env->has_global(var)) {
          Expression* e = dynamic_cast<Expression*>(env->get_global(var));
          if (!e || e->concrete_type() == Expression::NULL_VAL) {
            env->set_global(var, a->value()->perform(eval.snapshot()));
          }
        }
        else {
          env->set_global(var, a->value()->perform(eval.snapshot()));
        }
      }
      else {
        env->set_global(var, a->value()->perform(eval.snapshot()));
      }
    }
    else if (a->is_default()) {
      if (env->has_lexical(var)) {
        auto cur = env;
        while (cur && cur->is_lexical()) {
          if (cur->has_local(var)) {
            if (AST_Node* node = cur->get_local(var)) {
              Expression* e = dynamic_cast<Expression*>(node);
              if (!e || e->concrete_type() == Expression::NULL_VAL) {
                cur->set_local(var, a->value()->perform(eval.snapshot()));
              }
            }
            else {
              throw runtime_error("Env not in sync");
            }
            return 0;
          }
          cur = cur->parent();
        }
        throw runtime_error("Env not in sync");
      }
      else if (env->has_global(var)) {
        if (AST_Node* node = env->get_global(var)) {
          Expression* e = dynamic_cast<Expression*>(node);
          if (!e || e->concrete_type() == Expression::NULL_VAL) {
            env->set_global(var, a->value()->perform(eval.snapshot()));
          }
        }
      }
      else if (env->is_lexical()) {
        env->set_local(var, a->value()->perform(eval.snapshot()));
      }
      else {
        env->set_local(var, a->value()->perform(eval.snapshot()));
      }
    }
    else {
      env->set_lexical(var, a->value()->perform(eval.snapshot()));
    }
    return 0;
  }

  Statement* Expand::operator()(Import* imp)
  {
    Import* result = new (ctx.mem) Import(imp->pstate());
    for ( size_t i = 0, S = imp->urls().size(); i < S; ++i) {
      result->urls().push_back(imp->urls()[i]->perform(eval.snapshot()));
    }
    return result;
  }

  Statement* Expand::operator()(Import_Stub* i)
  {
    append_block(ctx.style_sheets[i->file_name()]);
    return 0;
  }

  Statement* Expand::operator()(Warning* w)
  {
    // eval handles this too, because warnings may occur in functions
    w->perform(eval.snapshot());
    return 0;
  }

  Statement* Expand::operator()(Error* e)
  {
    // eval handles this too, because errors may occur in functions
    e->perform(eval.snapshot());
    return 0;
  }

  Statement* Expand::operator()(Debug* d)
  {
    // eval handles this too, because warnings may occur in functions
    d->perform(eval.snapshot());
    return 0;
  }

  Statement* Expand::operator()(Comment* c)
  {
    // TODO: eval the text, once we're parsing/storing it as a String_Schema
    return new (ctx.mem) Comment(c->pstate(), static_cast<String*>(c->text()->perform(eval.snapshot())), c->is_important());
  }

  Statement* Expand::operator()(If* i)
  {
    if (*i->predicate()->perform(eval.snapshot())) {
      append_block(i->consequent());
    }
    else {
      Block* alt = i->alternative();
      if (alt) append_block(alt);
    }
    return 0;
  }

  // For does not create a new env scope
  // But iteration vars are reset afterwards
  Statement* Expand::operator()(For* f)
  {
    string variable(f->variable());
    Expression* low = f->lower_bound()->perform(eval.snapshot());
    if (low->concrete_type() != Expression::NUMBER) {
      error("lower bound of `@for` directive must be numeric", low->pstate(), backtrace());
    }
    Expression* high = f->upper_bound()->perform(eval.snapshot());
    if (high->concrete_type() != Expression::NUMBER) {
      error("upper bound of `@for` directive must be numeric", high->pstate(), backtrace());
    }
    Number* sass_start = static_cast<Number*>(low);
    Number* sass_end = static_cast<Number*>(high);
    // check if units are valid for sequence
    if (sass_start->unit() != sass_end->unit()) {
      stringstream msg; msg << "Incompatible units: '"
        << sass_start->unit() << "' and '"
        << sass_end->unit() << "'.";
      error(msg.str(), low->pstate(), backtrace());
    }
    double start = sass_start->value();
    double end = sass_end->value();
    // only create iterator once in this environment
    Env* env = environment();
    Number* it = new (env->mem) Number(low->pstate(), start, sass_end->unit());
    AST_Node* old_var = env->has_local(variable) ? env->get_local(variable) : 0;
    env->set_local(variable, it);
    Block* body = f->block();
    if (start < end) {
      if (f->is_inclusive()) ++end;
      for (double i = start;
           i < end;
           ++i) {
        it->value(i);
        env->set_local(variable, it);
        append_block(body);
      }
    } else {
      if (f->is_inclusive()) --end;
      for (double i = start;
           i > end;
           --i) {
        it->value(i);
        env->set_local(variable, it);
        append_block(body);
      }
    }
    // restore original environment
    if (!old_var) env->del_local(variable);
    else env->set_local(variable, old_var);
    return 0;
  }

  // Eval does not create a new env scope
  // But iteration vars are reset afterwards
  Statement* Expand::operator()(Each* e)
  {
    vector<string> variables(e->variables());
    Expression* expr = e->list()->perform(eval.snapshot());
    List* list = 0;
    Map* map = 0;
    if (expr->concrete_type() == Expression::MAP) {
      map = static_cast<Map*>(expr);
    }
    else if (expr->concrete_type() != Expression::LIST) {
      list = new (ctx.mem) List(expr->pstate(), 1, List::COMMA);
      *list << expr;
    }
    else {
      list = static_cast<List*>(expr);
    }
    // remember variables and then reset them
    Env* env = environment();
    vector<AST_Node*> old_vars(variables.size());
    for (size_t i = 0, L = variables.size(); i < L; ++i) {
      old_vars[i] = env->has_local(variables[i]) ? env->get_local(variables[i]) : 0;
      env->set_local(variables[i], 0);
    }
    Block* body = e->block();

    if (map) {
      for (auto key : map->keys()) {
        Expression* k = key->perform(eval.snapshot());
        Expression* v = map->at(key)->perform(eval.snapshot());

        if (variables.size() == 1) {
          List* variable = new (ctx.mem) List(map->pstate(), 2, List::SPACE);
          *variable << k;
          *variable << v;
          env->set_local(variables[0], variable);
        } else {
          env->set_local(variables[0], k);
          env->set_local(variables[1], v);
        }
        append_block(body);
      }
    }
    else {
      for (size_t i = 0, L = list->length(); i < L; ++i) {
        List* variable = 0;
        if ((*list)[i]->concrete_type() != Expression::LIST  || variables.size() == 1) {
          variable = new (ctx.mem) List((*list)[i]->pstate(), 1, List::COMMA);
          *variable << (*list)[i];
        }
        else {
          variable = static_cast<List*>((*list)[i]);
        }
        for (size_t j = 0, K = variables.size(); j < K; ++j) {
          if (j < variable->length()) {
            env->set_local(variables[j], (*variable)[j]->perform(eval.snapshot()));
          }
          else {
            env->set_local(variables[j], new (ctx.mem) Null(expr->pstate()));
          }
        }
        append_block(body);
      }
    }
    // restore original environment
    for (size_t j = 0, K = variables.size(); j < K; ++j) {
      if(!old_vars[j]) env->del_local(variables[j]);
      else env->set_local(variables[j], old_vars[j]);
    }
    return 0;
  }

  Statement* Expand::operator()(While* w)
  {
    Expression* pred = w->predicate();
    Block* body = w->block();
    while (*pred->perform(eval.snapshot())) {
      append_block(body);
    }
    return 0;
  }

  Statement* Expand::operator()(Return* r)
  {
    error("@return may only be used within a function", r->pstate(), backtrace());
    return 0;
  }

  Statement* Expand::operator()(Extension* e)
  {
    To_String to_string(&ctx);
    Selector_List* extender = static_cast<Selector_List*>(selector_stack.back());
    if (!extender) return 0;
    selector_stack.push_back(0);

    // Contextualize contextual(eval.snapshot());
    Selector_List* selector_list = static_cast<Selector_List*>(e->selector());
    Selector_List* contextualized = static_cast<Selector_List*>(selector_list->perform(eval.snapshot()));
    // ToDo: remove once feature proves stable!
    // if (contextualized->length() != 1) {
    //   error("selector groups may not be extended", extendee->pstate(), backtrace());
    // }
    for (auto complex_sel : contextualized->elements()) {
      Complex_Selector* c = complex_sel;
      if (!c->head() || c->tail()) {
        error("nested selectors may not be extended", c->pstate(), backtrace());
      }
      Compound_Selector* compound_sel = c->head();
      compound_sel->is_optional(selector_list->is_optional());
      // // need to convert the compound selector into a by-value data structure
      // vector<string> target_vec;
      // for (size_t i = 0, L = compound_sel->length(); i < L; ++i)
      // { target_vec.push_back((*compound_sel)[i]->perform(&to_string)); }
      for (size_t i = 0, L = extender->length(); i < L; ++i) {
        // let's test this out
        // cerr << "REGISTERING EXTENSION REQUEST: " << (*extender)[i]->perform(&to_string) << " <- " << compound_sel->perform(&to_string) << endl;
        ctx.subset_map.put(compound_sel->to_str_vec(), make_pair((*extender)[i], compound_sel));
      }
    }
    selector_stack.pop_back();
    return 0;
  }

  Statement* Expand::operator()(Definition* d)
  {
    Env* env = environment();
    Definition* dd = new (ctx.mem) Definition(*d);
    env->local_frame()[d->name() +
                        (d->type() == Definition::MIXIN ? "[m]" : "[f]")] = dd;
    // set the static link so we can have lexical scoping
    dd->environment(env);
    return 0;
  }

  Statement* Expand::operator()(Mixin_Call* c)
  {
    Env* env = environment();
    string full_name(c->name() + "[m]");
    if (!env->has(full_name)) {
      error("no mixin named " + c->name(), c->pstate(), backtrace());
    }
    Definition* def = static_cast<Definition*>((*env)[full_name]);
    Block* body = def->block();
    Parameters* params = def->parameters();

    Arguments* args = static_cast<Arguments*>(c->arguments()
                                               ->perform(eval.snapshot()));
    Backtrace new_bt(backtrace(), c->pstate(), ", in mixin `" + c->name() + "`");
    backtrace_stack.push_back(&new_bt);
    Env new_env(def->environment());
    env_stack.push_back(&new_env);
    if (c->block()) {
      // represent mixin content blocks as thunks/closures
      Definition* thunk = new (ctx.mem) Definition(c->pstate(),
                                                   "@content",
                                                   new (ctx.mem) Parameters(c->pstate()),
                                                   c->block(),
                                                   &ctx,
                                                   Definition::MIXIN);
      thunk->environment(env);
      new_env.local_frame()["@content[m]"] = thunk;
    }
    bind("mixin " + c->name(), params, args, ctx, &new_env, eval.snapshot());
    append_block(body);
    backtrace_stack.pop_back();
    env_stack.pop_back();
    return 0;
  }

  Statement* Expand::operator()(Content* c)
  {
    Env* env = environment();
    // convert @content directives into mixin calls to the underlying thunk
    if (!env->has("@content[m]")) return 0;
    Mixin_Call* call = new (ctx.mem) Mixin_Call(c->pstate(),
                                                "@content",
                                                new (ctx.mem) Arguments(c->pstate()));
    return call->perform(this);
  }

  // produce an error if something is not implemented
  inline Statement* Expand::fallback_impl(AST_Node* n)
  {
    string err = string("`Expand` doesn't handle ") + typeid(*n).name();
    String_Constant* msg = new (ctx.mem) String_Constant(ParserState("[WARN]"), err);
    error("unknown internal error; please contact the LibSass maintainers", n->pstate(), backtrace());
    return new (ctx.mem) Warning(ParserState("[WARN]"), msg);
  }

}
