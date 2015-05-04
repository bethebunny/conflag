#include "conflag.h"
#include "parser.h"
#include "conflag.tab.hh"
#include <algorithm>
#include <cstdlib>
#include <cstdio>
#include <map>

using namespace std;

namespace conflag {

bref BaseValue::value() const {
  return shared_from_this();
}

bref Call::value() const {
  auto function = dynamic_pointer_cast<const Function>(func->value());
  if (!function) { throw TypeError("Attempted to call non-function"); }
  return function->call(args);
}

bref Plus::value() const {
  return a->value()->plus(b->value());
}

bref Object::resolve(string name) const {
  auto result = scope.find(name);
  if (result == scope.end()) {
    if (!parent) { throw NameResolutionError(name); }
    return parent->resolve(name);
  }
  return result->second->value();
}

bref Object::operator[](string name) const {
  auto result = scope.find(name);
  if (result == scope.end()) { throw NameResolutionError(name); }
  return result->second->value();
}

bref Name::value() const {
  return scope->resolve(name);
}

bref Access::value() const {
  auto scope = dynamic_pointer_cast<const Object>(expression->value());
  if (!scope) throw TypeError("Attempted to access attribute of non-object");
  return (*scope)[name];
}

bref Function::call(const vector<vref>& args) const {
  if (args.size() != node->args.size()) {
    throw TypeError("Incorrect number of arguments to function");
  }
  map<string, vref> argmap;
  size_t index = 0;
  for (auto const& arg : node->args) {
    argmap[arg] = args[index++];
  }
  auto call_scope = make_shared<Object>(argmap, scope);
  return node->expression->node_value(call_scope)->value();
}

bref NativeFunction::call(const vector<vref>& args) const {
  if (args.size() != static_cast<size_t>(num_args)) {
    throw TypeError("incorrect number of args to native function");
  }
  return func(args);
}


bref BaseValue::plus(bref) const {
  throw TypeError("Incompatible types for +");
}

bref Array::plus(bref other) const {
  auto a = dynamic_pointer_cast<const Array>(other);
  if (!a) { throw TypeError("Incompatible types for +"); }
  vector<vref> v(items);
  v.insert(v.end(), a->items.begin(), a->items.end());
  return make_shared<const Array>(move(v));
}

bref Object::plus(bref other) const {
  auto o = dynamic_pointer_cast<const Object>(other);
  if (!o) { throw TypeError("Incompatible types for +"); }
  map<string, vref> m(scope);
  m.insert(o->scope.begin(), o->scope.end());
  return make_shared<const Object>(move(m), nullptr);
}

bref String::plus(bref other) const {
  auto o = dynamic_pointer_cast<const String>(other);
  if (!o) { throw TypeError("Incompatible types for +"); }
  return make_shared<const String>(val + o->val);
}

bref Integer::plus(bref other) const {
  auto i = dynamic_pointer_cast<const Integer>(other);
  if (!i) {
    auto d = dynamic_pointer_cast<const Double>(other);
    if (!d) { throw TypeError("Incompatible types for +"); }
    return make_shared<const Double>(val + d->val);
  }
  return make_shared<const Integer>(val + i->val);
}

bref Double::plus(bref other) const {
  auto n = dynamic_pointer_cast<const Number>(other);
  if (!n) throw TypeError("Incompatible types for +");
  return make_shared<const Double>(val + static_cast<double>(*n));
}

Double::operator double() const {
  return val;
}

Integer::operator double() const {
  return val;
}

bool Value::equals(vref other) const {
  return this == other.get();
}

bool Object::equals(vref other) const {
  auto o = dynamic_pointer_cast<const Object>(other);
  auto end = o->scope.end();
  return o && (scope.size() == o->scope.size()) &&
    all_of(scope.begin(), scope.end(), [&o, &end](auto const& p) {
    //TODO: Either compare values or don't have objects be comparable probs
    auto f = o->scope.find(p.first);
    return f != end && p.second->equals(f->second);
  });
}

bool Array::equals(vref other) const {
  auto a = dynamic_pointer_cast<const Array>(other);
  if (!a) { return false; }
  if (items.size() != a->items.size()) { return false; }
  for (auto i = items.begin(), j = a->items.begin(), end = items.end();
      i != end; ++i, ++j) {
    //TODO: Either compare values or don't have arrays be comparable probs
    if (!(*i)->equals(*j)) { return false; }
  }
  return true;
}

bool String::equals(vref other) const {
  auto s = dynamic_pointer_cast<const String>(other);
  return s && (val == s->val);
}

bool Number::equals(vref other) const {
  auto n = dynamic_pointer_cast<const Number>(other);
  return n && static_cast<double>(*this) == static_cast<double>(*n);
}

bool Boolean::equals(vref other) const {
  auto b = dynamic_pointer_cast<const Boolean>(other);
  return b && (val == b->val);
}

bool Null::equals(vref other) const {
  return static_cast<bool>(dynamic_pointer_cast<const Null>(other));
}



ObjectNode::ObjectNode(const vector<pair<string, aref>>& items) {
  this->items = map<string, aref>(items.begin(), items.end());
}

vref ObjectNode::node_value(Scope parent) const {
  auto new_scope = make_shared<Object>(map<string, vref>(), parent);
  for (auto const& i : items) {
    new_scope->scope[i.first] = i.second->node_value(new_scope);
  }
  return new_scope;
}

vref ArrayNode::node_value(Scope scope) const {
  return make_shared<Array>(f_map(items, [&scope](auto const& i) {
    return i->node_value(scope);
  }));
}

vref FunctionNode::node_value(Scope scope) const {
  return make_shared<Function>(shared_from_this(), scope);
}

vref CallNode::node_value(Scope scope) const {
  return make_shared<Call>(func->node_value(scope),
      f_map(args, [&scope](auto const& i) {
    return i->node_value(scope);
  }));
}

vref PlusNode::node_value(Scope scope) const {
  return make_shared<Plus>(a->node_value(scope), b->node_value(scope));
}

vref AccessNode::node_value(Scope scope) const {
  return make_shared<Access>(expression->node_value(scope), name);
}

vref NameNode::node_value(Scope scope) const {
  return make_shared<Name>(name, scope);
}


aref make_number_node(string str) {
  if (str.find(".") != string::npos) {
    return make_shared<DoubleNode>(atof(str.c_str()));
  } else {
    return make_shared<IntegerNode>(atol(str.c_str()));
  }
}


//TODO: Clean this up, virtual print operators
ostream& operator<<(ostream &stream, vref v) {
  auto o = dynamic_pointer_cast<const Object>(v);
  if (o) {
    stream << "{";
    for (auto p : o->scope) {
      stream << " " << p.first << ": " << p.second << ",";
    }
    return stream << "}";
  }
  auto ar = dynamic_pointer_cast<const Array>(v);
  if (ar) {
    stream << "[";
    for (auto i : ar->items) {
      stream << " " << i << ",";
    }
    return stream << "]";
  }
  auto ac = dynamic_pointer_cast<const Access>(v);
  if (ac) { return stream << ac->expression << ".\"" << ac->name << "\""; }
  auto n = dynamic_pointer_cast<const Name>(v);
  if (n) { return stream << n->name; }
  auto p = dynamic_pointer_cast<const Plus>(v);
  if (p) { return stream << "(" << p->a << ") + (" << p->b << ")"; }
  auto s = dynamic_pointer_cast<const String>(v);
  if (s) { return stream << "\"" << s->val << "\""; };
  auto i = dynamic_pointer_cast<const Integer>(v);
  if (i) { return stream << i->val; };
  auto d = dynamic_pointer_cast<const Double>(v);
  if (d) { return stream << d->val; };
  auto b = dynamic_pointer_cast<const Boolean>(v);
  if (b) { return stream << ((*b) ? "true" : "false"); }
  auto f = dynamic_pointer_cast<const Function>(v);
  if (f) {
    auto fn = f->node;
    stream << "(_";
    for (auto arg : fn->args) {
      stream << " " << arg;
    }
    stream << ": " << fn->expression->node_value(make_shared<Object>()) << ")";
    return stream;
  };
  auto c = dynamic_pointer_cast<const Call>(v);
  if (c) {
    stream << c->func;
    for (auto arg : c->args) {
      stream << " " << arg;
    }
    return stream;
  }
  auto null = dynamic_pointer_cast<const Null>(v);
  if (null) { return stream << "null"; }
  return stream;
}

ostream& operator<<(ostream &stream, bref v) {
  return stream << dynamic_pointer_cast<const Value>(v);
}


static bref builtin_reduce(vector<vref> args) {
  auto func = dynamic_pointer_cast<const Function>(args[0]->value());
  if (!func) { throw TypeError("builtin reduce requires function as first arg"); }
  auto array = dynamic_pointer_cast<const Array>(args[1]->value());
  if (!array) { throw TypeError("builtin reduce requires array as second arg"); }
  auto result = args[2]->value();
  for (auto const& i : array->items) {
    result = func->call(vector<vref>{result, i->value()});
  }
  return result;
}

static bref builtin_map(vector<vref> args) {
  auto func = dynamic_pointer_cast<const Function>(args[0]->value());
  if (!func) { throw TypeError("builtin map requires function as first arg"); }
  auto array = dynamic_pointer_cast<const Array>(args[1]->value());
  if (!array) { throw TypeError("builtin map requires array as second arg"); }
  return make_shared<Array>(f_map(array->items,
        [&func](auto const& i) -> vref {
    return func->call(vector<vref>{i->value()});
  }));
}

//TODO: probably can have these return vrefs
static bref builtin_if(vector<vref> args) {
  //TODO: false-y stuff
  auto b = dynamic_pointer_cast<const Boolean>(args[0]->value());
  if (!b) { throw TypeError("builtin if requires boolean as first arg"); }
  return (*b) ? args[1]->value() : args[2]->value();
}

static bref builtin_equals(vector<vref> args) {
  return make_shared<Boolean>(args[0]->value()->equals(args[1]->value()));
}

static bref builtin_import(vector<vref> args) {
  auto s = dynamic_pointer_cast<const String>(args[0]->value());
  if (!s) { throw TypeError("builtin import requires string as first arg"); }
  return load(s->val);
}

static bref builtin_object(vector<vref> args) {
  auto array = dynamic_pointer_cast<const Array>(args[1]->value());
  if (!array) { throw TypeError("builtin object requires array as first arg"); }
  map<string, vref> scope;
  for (auto const& item : array->items) {
    auto iarray = dynamic_pointer_cast<const Array>(item->value());
    if (!iarray || iarray->items.size() != 2) {
      throw TypeError(
          "builtin object must take an array of [string, val] pairs");
    }
    auto name = dynamic_pointer_cast<const String>(iarray->items[0]);
    if (!name) {
      throw TypeError(
          "builtin object must take an array of [string, val] pairs");
    }
    scope[name->val] = iarray->items[1];
  }
  return make_shared<Object>(scope, nullptr);
}

Boolean::operator bool() const {
  return val;
}


Scope builtins() {
  map<string, vref> scope;
  scope["reduce"] = make_shared<NativeFunction>(3, builtin_reduce);
  scope["map"] = make_shared<NativeFunction>(2, builtin_map);
  scope["if"] = make_shared<NativeFunction>(3, builtin_if);
  scope["equals"] = make_shared<NativeFunction>(2, builtin_equals);
  scope["import"] = make_shared<NativeFunction>(1, builtin_import);
  scope["object"] = make_shared<NativeFunction>(1, builtin_object);
  scope["true"] = make_shared<Boolean>(true)->value();
  scope["false"] = make_shared<Boolean>(false)->value();
  scope["null"] = make_shared<Null>()->value();
  return make_shared<Object>(scope, nullptr);
}


aref parse_output;
thread_local void *yyguts;

} /* conflag */
