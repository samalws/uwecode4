tokens = ("VAR", "MIDVAR", "SEMI", "COLONEQUALS", "ARROW", "LPAREN", "RPAREN", "STRINGLIT")

t_VAR = r'[a-zA-Z0-9\!\@\#\$\%\^\&\*\+\=\_\|\'\<\>\,\.\/\?]+'
t_MIDVAR = r'`[a-zA-Z0-9\!\@\#\$\%\^\&\*\+\=\_\|\'\<\>\,\.\/\?]+'
t_SEMI = r';'
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

def p_code_null(t):
  'code : '
  t[0] = []

def p_code_cons(t):
  'code : statement SEMI code'
  t[0] = [t[1]] + t[3]

def p_statement_assign(t):
  'statement : VAR COLONEQUALS expr'
  t1 = t[1]
  t3 = t[3]
  def t0(m):
    m[t1] = t3(m)
  t[0] = t0

def p_expr(t):
  'expr : lvl1expr'
  t[0] = t[1]

def p_lvl1expr_lam(t):
  'lvl1expr : VAR ARROW expr'
  t1 = t[1]
  t3 = t[3]
  def t0(m, x):
    m2 = m.copy()
    m2[t1] = x
    return t3(m2)()
  t[0] = lambda m: lambda: lambda x: t0(m,x)

def p_lvl1expr_lvl2expr(t):
  'lvl1expr : lvl2expr'
  t[0] = t[1]

def p_lvl2expr_midapp(t):
  'lvl2expr : lvl3expr MIDVAR lvl2expr'
  t1 = t[1]
  t2 = t[2]
  t3 = t[3]
  t[0] = lambda m: fnCall(fnCall(m[t2[1:]],t1(m)),t3(m))

def p_lvl2expr_lvl3expr(t):
  'lvl2expr : lvl3expr'
  t[0] = t[1]

def p_lvl3expr_app(t):
  'lvl3expr : lvl3expr lvl4expr'
  t1 = t[1]
  t2 = t[2]
  t[0] = lambda m: fnCall(t1(m),t2(m))

def p_lvl3expr_lvl4expr(t):
  'lvl3expr : lvl4expr'
  t[0] = t[1]

def p_lvl4expr_var(t):
  'lvl4expr : VAR'
  t1 = t[1]
  t[0] = lambda m: m[t1]

def p_lvl4expr_parens(t):
  'lvl4expr : LPAREN expr RPAREN'
  t[0] = t[2]

def p_lvl4expr_string(t):
  'lvl4expr : STRINGLIT'
  t1 = t[1]
  t[0] = lambda m: lambda: t1[1:-1]

def p_error(t):
  print(str(t.lineno) + ": Syntax error at " + t.value)
  exit(1)

import ply.yacc as yacc
parseCode = yacc.yacc(start="code")
parseExpr = yacc.yacc(start="expr")

def codeStrToTerm(s): # TODO do exprs instead of stmts
  parsed = parseCode.parse(s)
  m = {}
  for defn in parsed: defn(m)
  return m["main"]()

def exprStrToTerm(s):
  return parseExpr.parse(s)({})

def fnToTerm(f):
  return lambda: lambda x: f(x())

def twoFnToTerm(f):
  return lambda: lambda x: lambda y: f(x(),y())

toFeed = [
  lambda: True,
  lambda: False,
  fnToTerm(lambda s: int(s)),
  twoFnToTerm(lambda n, m: n+m),
  twoFnToTerm(lambda n, m: n*m),
  twoFnToTerm(lambda n, m: n**m),
  twoFnToTerm(lambda n, m: n/m),
  twoFnToTerm(lambda n, m: n//m),
  twoFnToTerm(lambda n, m: n % m),
  twoFnToTerm(lambda n, m: n == m),
  twoFnToTerm(lambda n, m: n < m),
  twoFnToTerm(lambda n, m: n and m),
  twoFnToTerm(lambda n, m: n or m),
  fnToTerm(lambda n: not n),
  fnToTerm(lambda n: exprStrToTerm("x -> y -> " + ("x" if n else "y"))()),
]

def runCode(code):
  main = codeStrToTerm(code)
  for feed in toFeed:
    main = main(feed)
  print(main)

runCode(open("example.uwe","r").read())
