import ast
import collections
import itertools
import ply
import ply.lex
import ply.yacc

tokens = [
        'COMMA', 'DOT', 'COLON', 'PLUS', 'LBRACE', 'RBRACE', 'LPAREN', 'RPAREN',
        'LBRACKET', 'RBRACKET', 'NAME', 'STRING', 'NUMBER', 'UNDERSCORE',
]

class ConflagError(Exception):
    def __str__(self):
        return '{}: {}'.format(type(self).__name__, self.message)

class ConflagSyntaxError(ConflagError):
    pass

class ConflagNameResolutionError(ConflagError):
    pass

class BadCall(ConflagError):
    def __init__(self, func, args):
        super(BadCall, self).__init__('{} needs {} arguments, got {}'.format(
            func.name, len(func.args), len(args)))

class Lexer(object):
    t_COMMA = ','
    t_DOT = '\.'
    t_COLON = ':'
    t_PLUS = r'\+'
    t_LBRACE = r'\{'
    t_RBRACE = r'\}'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_LBRACKET = r'\['
    t_RBRACKET = r'\]'
    t_UNDERSCORE = '_'

    t_NAME = '[A-Za-z_][A-Za-z0-9_]+|[A-Za-z]'
    t_NUMBER = r'-?\d+(?:\.\d+)?(?:[eE]\d+)?'

    states = (
            ('string', 'exclusive'),
            ('escaped', 'exclusive'),
    )

    tokens = tokens

    t_ignore = ' \t\r'
    t_string_ignore = ''
    t_escaped_ignore = ''

    t_ignore_comment = '//[^\n]*'

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count('\n')
        t.lexer.linestart = t.lexer.lexpos

    def t_error(self, t):
        raise ConflagSyntaxError('Illegal token "{}" on line {}:{}.'.format(
            t.value, t.lexer.lineno, t.lexer.lexpos - t.lexer.linestart))

    def t_escaped_CHAR(self, t):
        r'[\\/"bfnrt]'
        t.lexer.pop_state()
        t.lexer.string_val += unichr(ord(t))

    def t_escaped_UNICHAR(self, t):
        r'u[0-9a-f]{4}'
        t.lexer.pop_state()
        t.lexer.string_val += unichr(int(t.value[1:], 16))

    def t_escaped_error(self, t):
        raise ConflagSyntaxError(
                'Unrecognized escape character "{}" at ({}:{})'.format(
                    t.value, t.lexer.lineno, t.lexer.lexpos - t.lexer.linestart))

    def t_STARTSTRING(self, t):
        '"'
        t.lexer.string_val = ""
        t.lexer.push_state('string')

    def t_string_newline(self, t):
        r'\n'
        raise ConflagSyntaxError(
                'Unclosed string on line {}'.format(t.lexer.lineno))

    def t_string_CHARS(self, t):
        ur'[^\x00-\x19\\"]+'
        t.lexer.string_val += t.value

    def t_string_STRING(self, t):
        '"'
        t.lexer.pop_state()
        t.value = t.lexer.string_val
        return t

    def t_string_escape(self, t):
        r'\\'
        t.lexer.push_state('escaped')

    def t_string_error(self, t):
        raise ConflagSyntaxError(
                'Illegal string character "{}" at ({}:{})'.format(
                    t.value, t.lexer.lineno, t.lexer.lexpos - t.lexer.linestart))

    def lex(self, data, **kwargs):
        lexer = ply.lex.lex(module=self, **kwargs)
        lexer.input(data)
        return list(iter(lexer.token, None))

Statement = collections.namedtuple('Statement', 'name expression')

class AstNode(object):
    pass

class ObjectNode(AstNode):
    def __init__(self, items=()):
        self.items = dict(items)

    def value(self, scope):
        o = Object(scope)
        o.update((str(k), v.value(o)) for k, v in self.items.iteritems())
        return o

class ArrayNode(AstNode):
    def __init__(self, items=()):
        self.items = tuple(items)

    def value(self, scope):
        return Array(i.value(scope) for i in self.items)

class NameNode(AstNode):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def value(self, scope):
        return Name(self.name, scope)

class LiteralNode(AstNode):
    def __init__(self, value):
        self.val = value

class StringNode(LiteralNode):
    def value(self, scope):
        return String(self.val)

    def __str__(self):
        return self.val

class NumberNode(LiteralNode):
    def value(self, scope):
        return Number(ast.literal_eval(self.val))

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
        return Call(self.func.value(scope), [a.value(scope) for a in self.args])

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
        except KeyError:
            return self.parent_scope.resolve(item)

    def __contains__(self, item):
        try:
            self.get(item)
            return True
        except KeyError:
            return False

    def __getitem__(self, item):
        return self.get(item)

class NullScope(Scope):
    def resolve(self, item):
        raise ConflagNameResolutionError(item)


class Expression(object):
    def value(self):
        raise NotImplementedError(type(self))

class Primitive(Expression):
    def value(self):
        return self

class Function(Primitive):
    def __init__(self, func_node, scope):
        self.node = func_node
        self.args = [arg.name for arg in self.node.args]
        self.scope = scope

    def call(self, *args):
        if len(args) != len(self.args):
            raise ConflagError('Wrong number of arguments to function')
        call_scope = Object(self.scope, zip(self.args, args))
        return self.node.expression.value(call_scope)

    def __repr__(self):
        return repr(self.node.expression.value(self.scope))

    def native(self):
        return self

class Call(Expression):
    _value = None
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def value(self):
        if self._value is None:
            func = self.func.value()
            self._value = func.call(*self.args).value()
        return self._value

    def __repr__(self):
        return '({!r} {})'.format(
                self.func, ' '.join(repr(arg) for arg in self.args))

class Object(Primitive, Scope):
    _native = None

    def __init__(self, parent, items=()):
        self.items = dict(items)
        self.parent_scope = parent

    def update(self, items):
        self.items.update(items)

    def get(self, item):
        return self.items[item].value()

    def __add__(self, other):
        # Composite objects will never be referenced as scopes
        # so they don't need parents
        return Object(None, itertools.chain(self.items.iteritems(),
            other.items.iteritems()))

    def __eq__(self, other):
        return isinstance(other, Object) and self.items == other.items

    def __nonzero__(self):
        return bool(self.items)

    def __repr__(self):
        return '{{{}}}'.format(', '.join('{}: {!r}'.format(k, v.value())
            for k, v in self.items.iteritems()))

    def native(self):
        if self._native is None:
            obj = self
            class ConflagObject(object):
                def __getitem__(self, item):
                    return obj.get(item).native()
                __getattr__ = __getitem__
                __repr__ = obj.__repr__

            self._native = ConflagObject()
        return self._native

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

    def __repr__(self):
        return '({!r} + {!r})'.format(self.a, self.b)

class Array(Primitive):
    def __init__(self, items):
        self.items = list(items)

    def __iter__(self):
        for item in self.items:
            yield item.value()

    def __add__(self, other):
        return Array(itertools.chain(self.items, other.items))

    def __eq__(self, other):
        return isinstance(other, Array) and self.items == other.items

    def __nonzero__(self):
        return bool(self.items)

    def __repr__(self):
        return '[{}]'.format(', '.join(repr(i) for i in self.items))

    def native(self):
        return [i.value().native() for i in self.items]

class Reference(Expression):
    _value = None
    def __init__(self, expression, name):
        self.expression = expression
        self.name = name

    def value(self):
        if self._value is None:
            expr_scope = self.expression.value()
            self._value = expr_scope.get(str(self.name)).value()
        return self._value

    def __repr__(self):
        return '{!r}.{}'.format(self.expression, self.name)

class Name(Expression):
    _value = None
    def __init__(self, name, scope):
        self.name = name
        self.scope = scope

    def value(self):
        if self._value is None:
            self._value = self.scope.resolve(self.name).value()
        return self._value

    def __repr__(self):
        return self.name

class String(Primitive):
    def __init__(self, val):
        self.val = val

    def __eq__(self, other):
        return isinstance(other, String) and self.val == other.val

    def __add__(self, other):
        return String(self.val + other.val)

    def __nonzero__(self):
        return bool(self.val)

    def __str__(self):
        return self.val

    def __repr__(self):
        return '"{}"'.format(self.val)

    def native(self):
        return self.val

class Number(Primitive):
    def __init__(self, val):
        self.val = val

    def __eq__(self, other):
        return isinstance(other, Number) and self.val == other.val

    def __add__(self, other):
        return Number(self.val + other.val)

    def __nonzero__(self):
        return bool(self.val)

    def __repr__(self):
        return str(self.val)

    def native(self):
        return self.val

class Boolean(Primitive):
    def __init__(self, val=False):
        self.val = bool(val)

    def __nonzero__(self):
        return self.val

    def __repr__(self):
        return 'true' if self.val else 'false'

    def native(self):
        return self.val

class Null(Primitive):
    def __nonzero__(self):
        return False

    def __eq__(self, other):
        return isinstance(other, Null)

    def __repr__(self):
        return 'null'

    def native(self):
        return None

# BUILTINS

def builtin(func):
    func.call = func
    func.value = lambda: func
    return func

@builtin
def builtin_import(uri):
    with open(str(uri.value())) as res:
        return parse(res.read())

@builtin
def builtin_map(func, array):
    func = func.value()
    return Array(func.call(i) for i in array.value())

@builtin
def builtin_reduce(func, array, initial=None):
    func = func.value()
    return reduce(lambda x, y: func.call(x, y),
            array.value(), initial.value() if initial else initial)

@builtin
def builtin_filter(func, array):
    func = func.value()
    return Array(i for i in array if func.call(i))

@builtin
def builtin_if(pred, a, b):
    return a.value() if pred.value() else b.value()

@builtin
def builtin_equals(a, b):
    return Boolean(a.value() == b.value())

@builtin
def builtin_exists(a, b):
    try:
        return a.value()
    except ConflagNameResolutionError:
        return b.value()

BUILTIN_SCOPE = Object(NullScope(), {
    'import': builtin_import,
    'map': builtin_map,
    'reduce': builtin_reduce,
    'filter': builtin_filter,
    'if': builtin_if,
    'true': Boolean(True),
    'false': Boolean(False),
    'null': Null(),
    'equals': builtin_equals,
    'exists': builtin_exists,
})


class Parser(object):
    tokens = tokens

    def p_file(self, p):
        '''file : statements
                | object'''
        if not isinstance(p[1], ObjectNode):
            p[0] = ObjectNode(p[1])
        else:
            p[0] = p[1]

    def p_object(self, p):
        '''object : LBRACE statements RBRACE
                  | LBRACE RBRACE'''
        p[0] = ObjectNode(p[2] if len(p) > 3 else ())

    def p_statements(self, p):
        '''statements : statement COMMA statements
                      | statement COMMA
                      | statement'''
        p[0] = [p[1]] if len(p) <= 3 else ([p[1]] + p[3])

    def p_statement(self, p):
        '''statement : function
                     | name COLON expression
                     | string COLON expression'''
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = Statement(p[1], p[3])

    def p_names(self, p):
        '''names : name names
                 | name'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[2]

    def p_name(self, p):
        '''name : NAME
                | UNDERSCORE'''
        p[0] = NameNode(p[1])

    def p_expression(self, p):
        '''expression : name
                      | call
                      | literal
                      | object
                      | LBRACKET array RBRACKET
                      | LPAREN expression RPAREN
                      | LPAREN lambda RPAREN
                      | expression DOT name
                      | expression PLUS expression'''
        if len(p) == 2:
            p[0] = p[1]
        elif p[1] == '(':
            if isinstance(p[2], tuple):
                p[0] = p[2][1]
            else:
                p[0] = p[2]
        elif p[1] == '[':
            p[0] = ArrayNode(p[2])
        elif p[2] == '.':
            p[0] = ReferenceNode(p[1], p[3])
        else:
            p[0] = PlusNode(p[1], p[3])

    def p_expressions(self, p):
        '''expressions : expression expressions
                       | expression'''
        if len(p) == 2:
            if isinstance(p[1], list):
                p[0] = p[1]
            else:
                p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[2]

    def p_call(self, p):
        'call : expression expressions'
        p[0] = CallNode(p[1], p[2])

    def p_function(self, p):
        'function : name names COLON expression'
        p[0] = Statement(p[1], FunctionNode(p[2], p[4]))

    def p_lambda(self, p):
        'lambda : UNDERSCORE names COLON expression'
        self.p_function(p)

    def p_array(self, p):
        '''array : expression COMMA array
                 | expression
                 | '''
        if len(p) == 4:
            p[0] = [p[1]] + p[3]
        elif len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = []

    def p_literal(self, p):
        '''literal : number
                   | string'''
        p[0] = p[1]

    def p_number(self, p):
        'number : NUMBER'
        p[0] = NumberNode(p[1])

    def p_string(self, p):
        'string : STRING'
        p[0] = StringNode(p[1])

    def p_error(self, p):
        raise ConflagSyntaxError('Syntax error: {}'.format(p))

    def parse(self, data, **kwargs):
        parser = ply.yacc.yacc(module=self, **kwargs)
        lexer = ply.lex.lex(module=Lexer(), **kwargs)
        #TODO: use a real flag instead of python -O
        return parser.parse(data, lexer=lexer, debug=__debug__)

def parse(data):
    return Parser().parse(data).value(BUILTIN_SCOPE)

def loads(data):
    return parse(data).native()

if __name__ == '__main__':
    import sys
    cfg = parse(sys.stdin.read())
    print 'cfg:', cfg
    print 'horse beans:', cfg["horse beans"]
    print 'j2:', cfg["j2"]
    print 'j2.b:', cfg["j2"]["b"]
    print 'j2.c:', cfg["j2"]["c"]
    print 'd:', cfg["d"]
    print 'd2:', cfg["d2"]
    print 'e:', cfg["e"]
    print 'bools:', cfg["bools"]
    print 'iftest:', cfg["iftest"]
    print 'andtest:', cfg["andtest"]
    print 'functest:', cfg["functest"]
    print 'doubleref:', cfg["doubleref"]
    print 'containstest1:', cfg["containstest1"]
    print 'containstest2:', cfg["containstest2"]
    print 'j3.b:', cfg["j3"]["b"]
    try:
        print 'j3.d:', cfg['j3']['d']
    except ConflagNameResolutionError as e:
        print 'j3.d:', e
    try:
        print 'badref', cfg['badref']
    except ConflagNameResolutionError as e:
        print 'badref:', e
    try:
        print 'j2.d', cfg['j2']['d']
    except ConflagNameResolutionError as e:
        print 'j2.d:', e
    print 'subscope_ref', cfg['subscope_ref']
    print 'subscope_ref_test', cfg['subscope_ref_test']
    print 'subscope_ref_test2', cfg['subscope_ref_test2']
    print 'subscope_ref_test3', cfg['subscope_ref_test3']
    print 'subscope_ref_test4', cfg['subscope_ref']['b']
