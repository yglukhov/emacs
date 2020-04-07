import ../emacs, module

template `~`*(u: untyped{nkIdent}): Value {.used.} = makeSweetLispCall(u, nil)
template `~`*(u: untyped{nkAccQuoted}): Value {.used.} = makeSweetLispCall(u, nil)
template `~`*(u: untyped{nkCall}): Value {.used.} = makeSweetLispCall(u, nil)
template `~`*(u: untyped{nkCall}, doStmt: untyped{nkDo}): Value {.used.} = makeSweetLispCall(u, doStmt)
