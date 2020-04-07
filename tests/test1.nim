# import emacs/module
import emacs
import emacs/[utils, sugar]

implementModule(Plugin_is_GPL_compatible)


proc main() =
  discard ~provide(~mymod)

  proc setLeaderKeys(keys: string, f: proc() {.nimcall.}) =
    discard ~`spacemacs/set-leader-keys`(keys, makeInteractiveFunction(f))

  proc setLeaderKeys(keys: string, funcName: Value, f: proc() {.nimcall.}) =
    # let fn = theEnv.intern(funcName)
    discard ~fset(funcName, makeInteractiveFunction(f))
    discard ~`spacemacs/set-leader-keys`(keys, funcName)

  proc globalSetKey(keys: string, funcName: Value, f: proc() {.nimcall.}) =
    discard ~fset(funcName, makeInteractiveFunction(f))
    discard ~`global-set-key`(~kbd(keys), funcName)

  setLeaderKeys("on", ~`test-func-defined-in-nim`) do():
    echo "Nim func called!"
    discard ~message("This string will be displayed in status!")

  globalSetKey("C-c n n", ~`test-func-defined-in-nim`) do():
    echo "Nim func called!" # This will go to stdout
    discard ~message("This string will be displayed in status bar!")

main()
