tokens = ("VAR", "MIDVAR", "AT", "COLONEQUALS", "ARROW", "LPAREN", "RPAREN", "STRINGLIT")

t_VAR = r'[a-zA-Z0-9\!\#\$\%\^\&\*\+\=\_\|\;\'\<\>\,\.\/\?]+'
t_MIDVAR = r'`[a-zA-Z0-9\!\#\$\%\^\&\*\+\=\_\|\;\'\<\>\,\.\/\?]+'
t_AT = r'\@'
t_COLONEQUALS = r':='
t_ARROW = r'->'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_STRINGLIT = r'"[^"]*"'

t_ignore = " \t"

def t_newline(t):
  r'\n'
  t.lexer.lineno += 1

def t_comment(t):
  r'\[[^\]]*\]'
  pass

def t_error(t):
  print("Illegal character " + t.value[0] + " at line " + t.lexer.lineno)
  t.lexer.skip(1)

import ply.lex as lex
lexer = lex.lex()

precedence = (("right","ARROW"), ("right","MIDVAR"), ("left", "AT"))

# expression: lambda map from var to value: lambda: lambda fn arg: ret val
# values in map already have map applied to it, and are thunks

def thunkify(exprLambda):
  tbl = { "status": 0, "result": None }
  # status:
  # 0: never evalled yet
  # 1: in process of evalling
  # 2: have a result
  def retVal(tbl):
    status = tbl["status"]
    if status == 0:
      status = 1
      tbl["result"] = exprLambda()
      tbl["status"] = 2
    elif status == 1:
      raise Exception("Infinite loop")
    return tbl["result"]
  return lambda: retVal(tbl)

def fnCall(f,x):
  return thunkify(lambda: (f())(x))

def p_statement_assign(t):
  'statement : VAR COLONEQUALS expression'
  t1 = t[1]
  t3 = t[3]
  def t0(m):
    m[t1] = t3(m)
  t[0] = t0

def p_expression_var(t):
  'expression : VAR'
  t1 = t[1]
  t[0] = lambda m: m[t1]

def p_expression_midapp(t):
  'expression : expression MIDVAR expression'
  t1 = t[1]
  t2 = t[2]
  t3 = t[3]
  t[0] = lambda m: fnCall(fnCall(m[t2[1:]],t1(m)),t3(m))

def p_expression_app(t):
  'expression : expression AT expression'
  t1 = t[1]
  t3 = t[3]
  t[0] = lambda m: fnCall(t1(m),t3(m))

def p_expression_parens(t):
  'expression : LPAREN expression RPAREN'
  t[0] = t[2]

def p_expression_lam(t):
  'expression : VAR ARROW expression'
  t1 = t[1]
  t3 = t[3]
  def t0(m, x):
    m2 = m.copy()
    m2[t1] = x
    return t3(m2)
  t[0] = lambda m: lambda: lambda x: t0(m,x)

def p_expression_string(t):
  'expression : STRINGLIT'
  t1 = t[1]
  t[0] = lambda m: lambda: t1[1:-1]

def p_error(t):
  print("Syntax error at " + t.value)

import ply.yacc as yacc
parser = yacc.yacc()

s = "f := q -> q @ \"asdf\""
parsed = parser.parse(s)
m = {}
parsed(m)
print(m["f"]()(lambda: lambda x: x()[1:])())
