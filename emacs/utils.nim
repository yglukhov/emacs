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

proc makeInteractiveFunction*(f: proc() {.nimcall.}): Value =
  # Converts `f` into an interactive function which can
  # be set as keybinding handler in emacs
  evalWithArgs("""
  (let ((b $0))
    (lambda ()
      "Some comment"
      (interactive)
      (funcall b)
      ))
  """, f)
