# emacs
Create emacs modules in nim. Proof of concept.

# Sample
```nim
# mymod.nim

import emacs
import emacs/[utils, sugar]

implementModule(Plugin_is_GPL_compatible)

discard ~provide(~mymod)

proc globalSetKey(keys: string, funcName: Value, f: proc() {.nimcall.}) =
  discard ~fset(funcName, makeInteractiveFunction(f))
  discard ~`global-set-key`(~kbd(keys), funcName)

globalSetKey("C-c n n", ~`test-func-defined-in-nim`) do():
  echo "Nim func called!" # This will go to stdout
  discard ~message("This string will be displayed in status bar!")
```
```
nim c --app:lib --nomain -o:mymod.so mymod.nim
```
```lisp
;; Replace PATH_TO in the following line and add it to emacs config.
(require 'mymod (expand-file-name "PATH_TO/mymod.so"))
```
Now start emacs and press `C-c n n` to see status message from nim.
