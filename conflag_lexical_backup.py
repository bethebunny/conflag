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

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count('\n')

    def t_error(self, t):
        raise ConflagSyntaxError('Illegal token "{}" on line {}.'.format(
            t.value, t.lexer.lineno))

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
                'Unrecognized escape character "{}" on line {}'.format(
                    t.value, t.lexer.lineno))

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
                'Illegal string character "{}" on line {}'.format(
                    t.value, t.lexer.lineno))

    def lex(self, data, **kwargs):
        lexer = ply.lex.lex(module=self, **kwargs)
        lexer.input(data)
        return list(iter(lexer.token, None))

Statement = collections.namedtuple('Statement', 'name expression')

class Expression(object):
    def value(self):
        raise NotImplementedError(type(self))

class Primitive(Expression):
    def value(self):
        return self

class ConflagObject(Primitive):
    def __init__(self, parent, items=()):
        self.dict = dict(items)
        self.parent = parent

    def resolve(self, name):
        if name in self.dict:
            return self.dict[name]
        return self.parent.resolve(name)

    def update(self, items):
        self.dict.update(items)

    #TODO: self.parent here is probably wrong
    def __add__(self, other):
        return ConflagObject(
            self.parent, ((k, v) for k, v in itertools.chain(
                self.dict.iteritems(), other.dict.iteritems())))

    def __getitem__(self, item):
        try:
            return self.dict[item].value()
        except KeyError:
            raise ConflagNameResolutionError(item)

    def __nonzero__(self):
        return bool(self.dict)

    def __eq__(self, other):
        return self.dict == other.dict

    def __repr__(self):
        return '{{{}}}'.format(', '.join(
                '{!r}: {!r}'.format(k, v) for k, v in self.dict.iteritems()))

class NullScope(ConflagObject):
    def __init__(self):
        pass

    def resolve(self, name):
        raise ConflagNameResolutionError(name)

class Name(Expression):
    def __init__(self, name, scope):
        self.name = name
        self.scope = scope

    def value(self):
        return self.scope.resolve(self.name).value()

    def __eq__(self, other):
        return str(other) == self.name

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return str(self.name)

class Function(Primitive):
    def __init__(self, args, expression, scope):
        self.args = args
        self.expression = expression
        self.scope = scope
        self.calling = False

    def call(self, *args, **kwargs):
        #Warning: This isn't thread safe.
        if self.calling:
            raise ConflagError('Code attempted recursion, aborting.')
        try:
            self.calling = True
            self.scope.update(zip(self.args, args))
            return self.expression.value()
        finally:
            self.calling = False

    def __eq__(self, other):
        return other is self

    def __repr__(self):
        return '(_ {}: {})'.format(
                ' '.join(str(arg) for arg in self.args),
                self.expression)

class Plus(Expression):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def value(self):
        v1 = self.a.value()
        v2 = self.b.value()
        if type(v1) != type(v2):
            raise ConflagError('Inconsistent types in +: {}, {}'.format(
                type(v1), type(v2)))
        return v1 + v2

    def __repr__(self):
        return '{!r} + {!r}'.format(self.a, self.b)

class Call(Expression):
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def value(self):
        func = self.func.value()
        return func.call(*self.args)

    def __repr__(self):
        return '({} {})'.format(
                self.func, " ".join(str(arg) for arg in self.args))

class Reference(Expression):
    def __init__(self, expression, name):
        self.expression = expression
        self.name = name

    def value(self):
        expr_scope = self.expression.value()
        return expr_scope[self.name]

    def __repr__(self):
        return '{}.{}'.format(self.expression, self.name)

class Number(Primitive):
    def __init__(self, n):
        if isinstance(n, basestring):
            self.n = ast.literal_eval(n)
        else:
            self.n = n

    def __add__(self, other):
        return Number(self.n + other.n)

    def __nonzero__(self):
        return bool(self.n)

    def __eq__(self):
        return self.n == other.n

    def __repr__(self):
        return str(self.n)

class String(Primitive):
    def __init__(self, s):
        self.s = s

    def __str__(self):
        return self.s

    def __hash__(self):
        return hash(self.s)

    def __add__(self, other):
        return String(self.s + other.s)

    def __nonzero__(self):
        return bool(self.s)

    def __eq__(self, other):
        return str(self) == str(other)

    def __repr__(self):
        return '"{}"'.format(self.s)

class Array(Primitive):
    def __init__(self, items):
        self.items = items

    def __iter__(self):
        return iter(self.items)

    def __add__(self, other):
        return Array(self.items + other.items)

    def __nonzero__(self):
        return bool(self.s)

    def __eq__(self, other):
        return all(v1 == v2 for v1, v2 in itertools.izip(self, other))

    def __repr__(self):
        return '[{}]'.format(', '.join(repr(i) for i in self.items))

class Boolean(Primitive):
    def __init__(self, val=False):
        self.val = bool(val)

    def __nonzero__(self):
        return self.val

    def __repr__(self):
        return 'true' if self.val else 'false'

class Null(Primitive):
    def __repr__(self):
        return 'null'

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
    return Array([func.value().call(i) for i in array.value()])

@builtin
def builtin_reduce(func, array, initial=None):
    return reduce(func.value().call, array.value(),
                  initial.value() if initial else initial)

@builtin
def builtin_filter(func, array):
    return Array([i for i in array if func.value(i.value())])

@builtin
def builtin_if(pred, a, b):
    return a.value() if pred.value() else b.value()

@builtin
def builtin_equals(a, b):
    return Boolean(a.value() == b.value())

BuiltinScope = ConflagObject(NullScope(), {
    'import': builtin_import,
    'map': builtin_map,
    'reduce': builtin_reduce,
    'filter': builtin_filter,
    'if': builtin_if,
    'true': Boolean(True),
    'false': Boolean(False),
    'null': Null(),
    'equals': builtin_equals,
})


class Parser(object):
    tokens = tokens

    def __init__(self):
        self.scope_stack = [BuiltinScope]

    def p_file(self, p):
        '''file : new_scope statements
                | LBRACE new_scope statements RBRACE'''
        p[0] = scope = self.scope_stack.pop()
        statements = p[2] if len(p) < 4 else p[3]
        scope.update(statements)

    def p_object(self, p):
        '''object : LBRACE new_scope statements RBRACE
                  | LBRACE new_scope RBRACE'''
        p[0] = scope = self.scope_stack.pop()
        if len(p) > 4:
            scope.update(p[3])

    def p_statements(self, p):
        '''statements : statement COMMA statements
                      | statement COMMA
                      | statement'''
        p[0] = [p[1]] if len(p) <= 3 else ([p[1]] + p[3])

    def p_new_scope(self, p):
        'new_scope :'
        scope = ConflagObject(self.scope_stack[-1])
        self.scope_stack.append(scope)
        p[0] = scope

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
        p[0] = Name(p[1], self.scope_stack[-1])

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
            p[0] = Array(p[2])
        elif p[2] == '.':
            p[0] = Reference(p[1], p[3])
        else:
            p[0] = Plus(p[1], p[3])

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
        p[0] = Call(p[1], p[2])

    def p_function(self, p):
        'function : name names COLON new_scope expression'
        p[0] = (p[1], Function(p[2], p[5], self.scope_stack.pop()))

    def p_lambda(self, p):
        'lambda : UNDERSCORE names COLON new_scope expression'
        p[0] = (p[1], Function(p[2], p[5], self.scope_stack.pop()))

    def p_array(self, p):
        '''array : expression COMMA array
                 | expression'''
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = [p[1]] + p[3]

    def p_literal(self, p):
        '''literal : number
                   | string'''
        p[0] = p[1]

    def p_number(self, p):
        'number : NUMBER'
        p[0] = Number(p[1])

    def p_string(self, p):
        'string : STRING'
        p[0] = String(p[1])

    def p_error(self, p):
        raise ConflagSyntaxError('Syntax error: {}'.format(p))

    def parse(self, data, **kwargs):
        parser = ply.yacc.yacc(module=self, **kwargs)
        lexer = ply.lex.lex(module=Lexer(), **kwargs)
        return parser.parse(data, lexer=lexer)

def parse(data):
    return Parser().parse(data)

if __name__ == '__main__':
    import sys
    cfg = parse(sys.stdin.read())
    print 'cfg:', cfg
    print 'horse beans:', cfg['horse beans']
    print 'j2:', cfg['j2']
    print 'j2.b:', cfg['j2']['b']
    print 'j2.c:', cfg['j2']['c']
    print 'd:', cfg['d']
    print 'd2:', cfg['d2']
    print 'e:', cfg['e']
    print 'bools:', cfg['bools']
    print 'iftest:', cfg['iftest']
    print 'andtest:', cfg['andtest']
    print 'functest:', cfg['functest']
    print 'doubleref:', cfg['doubleref']
    print 'containstest1:', cfg['containstest1']
    print 'containstest2:', cfg['containstest2']
    print 'j3.b:', cfg['j3']['b']  #bug?: should be 5 or 3
    try:
        print 'j3.d:', cfg['j3']['d']  #todo: what should happen here?
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
    print 'subscope_ref.b', cfg['subscope_ref']['b']
