import macros
import ../emacs
import sugar

proc eval*(s: string): Value =
  let s = "(progn " & s & " )"
  ~eval(~car(~`read-from-string`(s)), 1)

proc evalWithArgs*(s: string, args: varargs[Value, toEmArg]): Value =
  # Evals lisp code in `s`, which can refer to `args` as `$0`, `$1`, etc.
  var e = "(lambda ("
  for i in 0 ..< args.len:
    e &= " $"
    e &= $i
  e &= ") "
  e &= s
  e &= " )"
  let f = eval(e)
  funcall(f, args)

proc parseStrFormat(s: string, openChar, closeChar: char): seq[string] =
  ## Returns a seq of parts where every odd part is string literal
  ## and every even part is expression.
  var curPart = ""
  var inExpr = false
  for c in s:
    if c == openChar:
      assert(not inExpr, "Unexpected " & openChar)
      result.add(curPart)
      curPart.setLen(0)
      inExpr = true
    elif c == closeChar:
      assert(inExpr, "Unexpected " & closeChar)
      result.add(curPart)
      curPart.setLen(0)
      inExpr = false
    else:
      curPart.add(c)
  assert(not inExpr, "No terminating " & closeChar)
  result.add(curPart)

macro lisp*(s: static[string]): untyped =
  var parts = parseStrFormat(s, '{', '}')
  var code = ""
  for i in 0 .. parts.high:
    if i mod 2 == 0:
      code &= parts[i]
    else:
      code &= "$" & $(i div 2)

  result = newCall(bindSym"evalWithArgs", newLit(code))
  for i in 0 .. parts.high:
    if i mod 2 != 0:
      result.add(parseExpr(parts[i]))

proc makeInteractiveFunctionAux(interactiveString: string, f: Value): Value =
  var interactiveString = interactiveString
  if interactiveString.len != 0:
    interactiveString = (~`prin1-to-string`(interactiveString)).to(string)
  var doc = lisp("(prin1-to-string (documentation {f}))").to(string)
  evalWithArgs("""
  (let ((b $0))
    (lambda (&rest args)
      """ & doc & """
      (interactive """ & interactiveString & """)
      (apply b args)
      ))
  """, f)

template makeInteractiveFunction*[T](interactiveString: string, f: T): Value =
  # Converts `f` into an interactive function which can
  # be set as keybinding handler in emacs
  makeInteractiveFunctionAux(interactiveString, makeFunction(theEnv, f))

template makeInteractiveFunction*(f: proc() {.nimcall.}): Value =
  # Converts `f` into an interactive function which can
  # be set as keybinding handler in emacs
  makeInteractiveFunction("", f)
