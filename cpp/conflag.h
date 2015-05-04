#ifndef __conflag_h
#define __conflag_h

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <list>

namespace conflag {

//TODO: explicit private sections, change private names to use underscore
//TODO: class ordering: typedefs ; structs ; constructors ; assignment operators
//          deleters ; other operators ; methods ; members
//TODO: const type& name -> type const& name

template<typename T, typename C>
auto f_map(std::vector<T> const& in, C const& op) ->
    std::vector<decltype(op(std::declval<T>()))> {
  std::vector<decltype(op(std::declval<T>()))> results;
  for (auto const& i : in) { results.push_back(op(i)); }
  return results;
}

struct ConflagError : public std::runtime_error {
  using runtime_error::runtime_error;
};

struct TypeError : public ConflagError {
  using ConflagError::ConflagError;
};

struct NameResolutionError : public ConflagError {
  using ConflagError::ConflagError;
};

class Value;
class BaseValue;
class Object;
class AstNode;
class FunctionNode;

typedef std::shared_ptr<const AstNode> aref;
typedef std::shared_ptr<const BaseValue> bref;
typedef std::shared_ptr<const Value> vref;
typedef std::shared_ptr<const Object> Scope;

class Value {
  public:
    virtual bref value() const = 0;
    virtual bool equals(vref other) const;
};

class BaseValue : public Value, public std::enable_shared_from_this<BaseValue> {
  public:
    virtual bref value() const;
    virtual bref plus(bref other) const;
};

class Object : public BaseValue {
  // The node construction needs to be able to get a reference to the object
  // scope for constructing the subsequent values to add to it.
  friend class ObjectNode;
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  Scope parent;
  std::map<std::string, vref> scope;
  public:
    template<typename T>
    Object(T&& scope, Scope const& parent) :
          parent(parent), scope(std::forward<T>(scope)) {};
    Object() {};
    bref resolve(std::string name) const;
    virtual bref plus(bref other) const;
    virtual bool equals(vref other) const;
    bref operator[](std::string name) const;
};

class Array : public BaseValue {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  public:
    std::vector<vref> items; //TODO
    template<typename T> Array(T&& items) : items(std::forward<T>(items)) {};
    virtual bref plus(bref other) const;
    virtual bool equals(vref other) const;
};

class String : public BaseValue {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  public:
    std::string val;
    template<typename T> String(T&& val) : val(std::forward<T>(val)) {};
    virtual bref plus(bref other) const;
    virtual bool equals(vref other) const;
};

class Number : public BaseValue {
  public:
    virtual bool equals(vref other) const;
    virtual operator double() const = 0;
};

class Integer : public Number {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  friend class Double;
  long val;
  public:
    Integer(long val) : val(val) {};
    virtual bref plus(bref other) const;
    operator double() const;
};

class Double : public Number {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  friend class Integer;
  double val;
  public:
    Double(double val) : val(val) {};
    virtual bref plus(bref other) const;
    operator double() const;
};

class Boolean : public BaseValue {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  public:
    Boolean(bool value) : val(value) {};
    bool val;
    virtual bool equals(vref other) const;
    operator bool() const;
};

class Null : public BaseValue {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  public:
    Null() {};
    virtual bool equals(vref other) const;
};

class Function : public BaseValue {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  std::shared_ptr<const FunctionNode> node;
  Scope scope;
  protected:
    Function() {};
  public:
    Function(std::shared_ptr<const FunctionNode> node, Scope scope) :
        node(node), scope(scope) {};
    virtual bref call(std::vector<vref> const& args) const;
};

class NativeFunction : public Function {
  short num_args;
  std::function<bref(std::vector<vref> const&)> func;
  public:
    NativeFunction(short num_args,
        std::function<bref(std::vector<vref> const&)> func)
        : num_args(num_args), func(func) {}
    virtual bref call(std::vector<vref> const& args) const;
};


class Call : public Value {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  vref func;
  std::vector<vref> args;
  public:
    template<typename T> Call(vref func, T&& args) :
        func(func), args(std::forward<T>(args)) {};
    bref value() const;
};

class Access : public Value {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  vref expression;
  std::string name;
  public:
    Access(vref expression, std::string name) :
        expression(expression), name(name) {};
    bref value() const;
};

class Plus : public Value {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  vref a, b;
  public:
    Plus(vref a, vref b) : a(a), b(b) {};
    bref value() const;
};

class Name : public Value {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  std::string name;
  Scope scope;
  public:
    Name(std::string name, Scope scope) : name(name), scope(scope) {};
    bref value() const;
};

std::ostream& operator<<(std::ostream &stream, vref v);
std::ostream& operator<<(std::ostream &stream, bref v);


class AstNode {
  public:
    virtual vref node_value(Scope scope) const = 0;
};

class ObjectNode : public AstNode {
  std::map<std::string, aref> items;
  public:
    ObjectNode() {};
    ObjectNode(const std::map<std::string, aref>& items) : items(items) {};
    ObjectNode(const std::vector<std::pair<std::string, aref>>& items);
    vref node_value(Scope scope) const;
};

class ArrayNode : public AstNode {
  std::vector<aref> items;
  public:
    ArrayNode() {};
    ArrayNode(const std::vector<aref>& items) : items(items) {};
    vref node_value(Scope scope) const;
};

class FunctionNode : public AstNode,
    public std::enable_shared_from_this<FunctionNode> {
  friend std::ostream& operator<<(std::ostream &stream, vref v);
  friend class Function;
  std::string name;
  std::vector<std::string> args;
  aref expression;
  public:
    FunctionNode(std::string name, const std::vector<std::string>& args,
        aref expression) : name(name), args(args), expression(expression) {};
    vref node_value(Scope scope) const;
};

class CallNode : public AstNode {
  aref func;
  std::vector<aref> args;
  public:
    CallNode(aref func, const std::vector<aref>& args) :
        func(func), args(args) {};
    vref node_value(Scope scope) const;
};

class PlusNode : public AstNode {
  aref a, b;
  public:
    PlusNode(aref a, aref b) : a(a), b(b) {};
    vref node_value(Scope scope) const;
};

class AccessNode : public AstNode {
  aref expression;
  std::string name;
  public:
    AccessNode(aref expression, std::string name) :
        expression(expression), name(name) {};
    vref node_value(Scope scope) const;
};

class NameNode : public AstNode {
  std::string name;
  public:
    NameNode(std::string name) : name(name) {};
    vref node_value(Scope scope) const;
};

template<typename T> class LiteralNode : public AstNode {
  std::shared_ptr<T> value;
  public:
    template<typename S> LiteralNode(S&& value) :
        value(std::make_shared<T>(std::forward<S>(value))) {};
    vref node_value(Scope) const { return value; }
};

class StringNode : public LiteralNode<String> {
  using LiteralNode::LiteralNode;
};
class DoubleNode : public LiteralNode<Double> {
  using LiteralNode::LiteralNode;
};
class IntegerNode : public LiteralNode<Integer> {
  using LiteralNode::LiteralNode;
};

aref make_number_node(std::string str);

extern aref parse_output;

Scope builtins();

// Defined in conflag.l
bref load(std::string);

} /* namespace conflag */

#endif /* __conflag_h */
