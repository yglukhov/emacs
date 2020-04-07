import ../emacs
import sugar

proc eval*(s: string): Value =
  ~eval(~car(~`read-from-string`(~format("(progn %s)", s))), 1)

proc makeInteractiveFunction*(f: proc() {.nimcall.}): Value =
  # Converts `f` into an interactive function which can
  # be set as keybinding handler in emacs
  let makeInteractiveFunction = eval("""
  (lambda (f)
    (let ((b f))
      (lambda ()
        "Some comment"
        (interactive)
        (funcall b)
        )))""")
  funcall(makeInteractiveFunction, f)
