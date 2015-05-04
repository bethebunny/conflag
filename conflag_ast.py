class AstNode(object):
    pass

class ObjectNode(AstNode):
    def __init__(self, items=()):
        self.items = dict(items)

    def value(self, scope):
        o = Object(scope)
        o.update((k, v.value(o)) for k, v in self.items.iteritems())
        return o

class ArrayNode(AstNode):
    def __init__(self, items=()):
        self.items = tuple(items)

    def value(self, scope):
        return Array(i.value(scope) for i in self.items)

class NameNode(AstNode):
    def __init__(self, name):
        self.name = name

    def value(self, scope):
        return Name(self.name, scope)

class LiteralNode(AstNode):
    def __init__(self, value):
        self.val = value

class StringNode(LiteralNode):
    def value(self, scope):
        return String(self.val)

class NumberNode(LiteralNode):
    def value(self, scope):
        return Number(self.val)

class FunctionNode(AstNode):
    def __init__(self, args, expression):
        self.args = args
        self.expression = expression

    def value(self, scope):
        return Function(self, scope)

class CallNode(AstNode):
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def value(self, scope):
        return Call(self.func.value(scope), [a.value(scope) for a in self.arg])

class PlusNode(AstNode):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def value(self, scope):
        return Plus(self.a.value(scope), self.b.value(scope))

class ReferenceNode(AstNode):
    def __init__(self, expression, name):
        self.expression = expression
        self.name = name

    def value(self, scope):
        return Reference(self.expression.value(scope), self.name)


class Scope(object):
    def resolve(self, item):
        try:
            return self.get(item)
        except ConflagNameResolutionError:
            return self.parent_scope.resolve(item)

    def __contains__(self, item):
        try:
            self.get(item)
            return True
        except ConflagNameResolutionError:
            return False

class NullScope(Scope):
    def resolve(self, item):
        raise ConflagNameResolutionError(item)


class Expression(object):
    def __init__(self, node, parent_scope):
        self.node = node
        self.parent_scope = parent_scope

    def value(self):
        raise NotImplementedError(type(self))

class Primitive(Expression):
    def value(self):
        return self

class Function(Primitive):
    def __init__(self, func_node, scope):
        self.node = func_node
        self.scope = scope

    def call(self, *args):
        call_scope = Object(self.scope, zip(self.node.args, args))
        return self.node.expression.value(call_scope)

class Call(Expression):
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def value(self):
        func = self.func.value()
        return func.call(*self.args)

class Object(Primitive, Scope):
    def __init__(self, parent, items=()):
        self.items = dict(items)
        self.parent_scope = parent

    def update(self, items):
        self.items.update(items)

    def get(self, item):
        try:
            return self.items[item]
        except KeyError:
            raise ConflagNameResolutionError(item)

    def __add__(self, other):
        # Composite objects will never be referenced as scopes
        # so they don't need parents
        return Object(None, itertools.chain(self.items.iteritems(),
            other.items.iteritems()))

    def __eq__(self, other):
        return isinstance(other, Object) and self.items == other.items

    def __nonzero__(self):
        return bool(self.val)

class Plus(Expression):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def value(self):
        a = self.a.value()
        b = self.b.value()
        if type(a) != type(b):
            raise ConflagError(
                    'incompatible types for +: {}, {}'.format(type(a), type(b)))
        return a + b

class Array(Primitive):
    def __init__(self, items):
        self.items = list(self.items)

    def __iter__(self):
        for item in self.node.items:
            yield item.value()

    def __add__(self, other):
        return Array(itertools.chain(self.items, other.items))

    def __eq__(self, other):
        return isinstance(other, Array) and self.items == other.items

    def __nonzero__(self):
        return bool(self.val)

class Reference(Expression):
    def __init__(self, expression, name):
        self.expression = expression
        self.name = name

    def value(self):
        expr_scope = self.expression.value()
        return expr_scope.get(self.node.name).value()

class Name(Expression):
    def __init__(self, name, scope):
        self.name = name
        self.scope = scope

    def value(self):
        return self.scope.resolve(self.name).value()

class String(Primitive):
    def __init__(self, val):
        self.val = val

    def __eq__(self, other):
        return isinstance(other, String) and self.val == other.val

    def __add__(self, other):
        return String(self.val + other.val)

    def __nonzero__(self):
        return bool(self.val)

class Number(Primitive):
    def __init__(self, val):
        self.val = val

    def __eq__(self, other):
        return isinstance(other, Number) and self.val == other.val

    def __add__(self, other):
        return Number(self.val + other.val)

    def __nonzero__(self):
        return bool(self.val)

class Boolean(Primitive):
    def __init__(self, val=False):
        self.val = bool(val)

    def __nonzero__(self):
        return self.val

class Null(Primitive):
    pass
