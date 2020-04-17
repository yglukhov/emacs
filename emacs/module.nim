
type
  Value* = ptr object

  Env* = Env25

  FuncallExit {.size: sizeof(cint).} = enum
    feReturn
    feSignal
    feThrow

  Function = proc(e: Env, nargs: int, values: UncheckedArray[Value], data: pointer): Value {.cdecl.}
  Finalizer = proc(d: pointer) {.cdecl.}

  Runtime* = ptr object
    size*: int
    private: pointer
    getEnvironment*: proc(r: Runtime): Env {.cdecl.}

  Env25* = ptr object
    # Structure size (for version checking).
    size*: int

    private: pointer

    makeGlobalRef*: proc(e: Env25, r: Value): Value {.cdecl.}
    freeGlobalRef*: proc(e: Env25, r: Value) {.cdecl.}

    nonLocalExitCheck*: proc(e: Env25): FuncallExit {.cdecl.}
    nonLocalExitClear*: proc(e: Env25) {.cdecl.}
    nonLocalExitGet*: proc(e: Env25, symbolOut, dataOut: Value): FuncallExit {.cdecl.}
    nonLocalExitSignal*: proc(e: Env25, symbolOut, dataOut: Value) {.cdecl.}
    nonLocalExitThrow*: proc(e: Env25, tag, value: Value) {.cdecl.}

    pMakeFunction*: proc(e: Env25, minArity, maxArity: int, f: Function, doc: cstring, data: pointer): Value {.cdecl.}
    pFuncall*: proc(e: Env25, function: Value, nargs: int, args: ptr Value): Value {.cdecl.}
    pIntern*: proc(e: Env25, symName: cstring): Value {.cdecl.}

    pTypeOf*: proc(e: Env25, value: Value): Value {.cdecl.}

    pIsNotNil*: proc(e: Env25, value: Value): bool {.cdecl.}

    pEq*: proc(e: Env25, a, b: Value): bool {.cdecl.}

    pExtractInt*: proc(e: Env25, a: Value): int64 {.cdecl.}
    pMakeInt*: proc(e: Env25, v: int64): Value {.cdecl.}

    pExtractFloat*: proc(e: Env25, a: Value): cdouble {.cdecl.}
    pMakeFloat*: proc(e: Env25, v: cdouble): Value {.cdecl.}

    pCopyStringContents*: proc(e: Env25, v: Value, buf: cstring, sz: var int): bool {.cdecl.}
    pMakeString*: proc(e: Env25, s: cstring, sz: int): Value {.cdecl.}

    makeUserPtr*: proc(e: Env25, f: Finalizer, d: pointer): Value {.cdecl.}
    getUserPtr*: proc(e: Env25, v: Value): pointer {.cdecl.}
    setUserPtr*: proc(e: Env25, v: Value, d: pointer) {.cdecl.}

    getUserFinalizer*: proc(e: Env25, v: Value): Finalizer {.cdecl.}
    setUserFinalizer*: proc(e: Env25, v: Value, f: Finalizer) {.cdecl.}

    pVecGet*: proc(e: Env25, v: Value, i: int): Value {.cdecl.}
    pVecSet*: proc(e: Env25, v: Value, i: int, o: Value) {.cdecl.}
    pVecSize*: proc(e: Env25, v: Value): int {.cdecl.}

proc getEnv*(r: Runtime): Env = r.getEnvironment(r)

proc makeFunction*(e: Env, minArity, maxArity: int, f: Function, doc: cstring, data: pointer): Value =
  e.pMakeFunction(e, minArity, maxArity, f, doc, data)

proc funcall*(e: Env, function: Value, args: openarray[Value]): Value =
  e.pFuncall(e, function, args.len, cast[ptr Value](args))

proc intern*(e: Env, symName: cstring): Value = e.pIntern(e, symName)

proc getType*(e: Env, value: Value): Value = e.pTypeOf(e, value)
proc isNotNil*(e: Env, value: Value): bool = e.pIsNotNil(e, value)
proc eq*(e: Env, a, b: Value): bool = e.pEq(e, a, b)

proc extractInt*(e: Env, a: Value): int64 = e.pExtractInt(e, a)
proc makeInt*(e: Env, i: int64): Value = e.pMakeInt(e, i)

proc extractFloat*(e: Env, a: Value): cdouble = e.pExtractFloat(e, a)
proc makeFloat*(e: Env, i: cdouble): Value = e.pMakeFloat(e, i)

proc copyStringContents(e: Env, v: Value, buf: cstring, sz: var int): bool =
  e.pCopyStringContents(e, v, buf, sz)

proc makeString*(e: Env, s: cstring, sz: int): Value = e.pMakeString(e, s, sz)
proc makeString*(e: Env, s: string): Value {.inline.} = e.makeString(s, s.len)

proc extractStringRaw*(e: Env, a: Value, s: var string): bool =
  var sz: int
  result = e.copyStringContents(a, nil, sz)
  if result:
    s.setLen(sz - 1)
    if sz > 1:
      result = e.copyStringContents(a, s, sz)

proc getStringRaw(e: Env, a: Value): string =
  if not extractStringRaw(e, a, result):
    assert(false)

proc symbolName*(e: Env, v: Value): Value =
  e.funcall(e.intern("symbol-name"), [v])

proc extractString*(e: Env, a: Value, s: var string) =
  let typ = e.getType(a)
  var r = false
  if e.eq(typ, e.intern("string")):
    r = extractStringRaw(e, a, s)
  elif e.eq(typ, e.intern("symbol")):
    let symName = e.symbolName(a)
    r = extractStringRaw(e, symName, s)
  else:
    let typName = e.getStringRaw(e.symbolName(typ))
    raise newException(ValueError, "Could not convert lisp " & typName & " to string")

  if not r:
    raise newException(ValueError, "Could not convert EValue to string")

proc vecGet*(e: Env, v: Value, i: int): Value = e.pVecGet(e, v, i)
proc vecSet*(e: Env, v: Value, i: int, o: Value) = e.pVecSet(e, v, i, o)
proc vecSize*(e: Env, v: Value): int = e.pVecSize(e, v)

