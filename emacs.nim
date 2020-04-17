import macros
import emacs/module

export Value

var theEnv*: Env

type Plugin_is_GPL_compatible* = object

template implementModule*(plugin_is_GPL_compatible_arg: typedesc[Plugin_is_GPL_compatible]) =
  # Declare mandatory GPL symbol.
  proc plugin_is_GPL_compatible() {.exportc, dynlib.} = discard

  var modInited: bool
  {.push stackTrace: off.}
  proc emacs_module_init(r: Runtime): cint {.exportc, dynlib.} =
    if modInited: return 0
    theEnv = getEnv(r)
    proc NimMain() {.importc.}
    NimMain()
  {.pop.}

proc listLength(e: Env, v: Value): int =
  e.extractInt(e.funcall(e.intern("list-length"), [v])).int

proc emToNim[T](e: Env, v: Value, r: var T)

proc extractListToSeq(e: Env, v: Value, s: var seq) =
  let sz = e.listLength(v)
  s.setLen(sz)
  let nth = e.intern("nth")
  for i in 0 ..< sz:
    let c = e.funcall(nth, [e.makeInt(i), v])
    e.emToNim(c, s[i])

proc extractSeq(e: Env, v: Value, s: var seq) =
  let typ = e.getType(v)
  if e.eq(typ, e.intern("cons")):
    extractListToSeq(e, v, s)
  else:
    var typName: string
    if typ.isNil:
      typName = "nil"
    else:
      e.extractString(typ, typName)
    raise newException(ValueError, "Can not convert lisp " & typName & " to seq")

proc tupleSize[T](): int {.compileTime.} =
  var o: T
  for f in fields(o): inc result

proc extractListToTuple[T](e: Env, v: Value, s: var T) =
  const sz = tupleSize[T]()
  if sz != e.listLength(v):
    raise newException(ValueError, "Could not convert list to tuple of different length")
  var i = 0
  let nth = e.intern("nth")
  for k, f in fieldPairs(s):
    let c = e.funcall(nth, [e.makeInt(i), v])
    e.emToNim(c, f)
    inc i

proc extractTuple(e: Env, v: Value, s: var tuple) =
  let typ = e.getType(v)
  if e.eq(typ, e.intern("cons")):
    extractListToTuple(e, v, s)
  else:
    var typName: string
    e.extractString(typ, typName)
    raise newException(ValueError, "Can not convert lisp " & typName & " to seq")

proc emToNim[T](e: Env, v: Value, r: var T) =
  when T is SomeInteger:
    r = T(e.extractInt(v))
  elif T is SomeFloat:
    r = T(e.extractFloat(v))
  elif T is string:
    e.extractString(v, r)
  elif T is seq:
    e.extractSeq(v, r)
  elif T is tuple:
    e.extractTuple(v, r)
  elif T is Value:
    r = v
  elif T is bool:
    r = e.isNotNil(v)
  else:
    {.error: "Unsupported type to convert from Emacs Value: " & $T.}

iterator arguments(formalParams: NimNode): tuple[idx: int, name, typ, default: NimNode] =
  formalParams.expectKind(nnkFormalParams)
  var iParam = 0
  for i in 1 ..< formalParams.len:
    let pp = formalParams[i]
    for j in 0 .. pp.len - 3:
      yield (iParam, pp[j], copyNimTree(pp[^2]), pp[^1])
      inc iParam

proc verifyArgs(nargs: int, args: UncheckedArray[Value], argsExpected: int, funcName: string): bool {.inline.} =
  # Do nothing. Arity should be checked by emacs runtime before calling us.
  true

proc parseArg[T](e: Env, nargs: int, args: UncheckedArray[Value], idx: int, argName: string, v: var T) =
  emToNim(e, args[idx], v)

proc parseVarargs[T](e: Env, nargs: int, args: UncheckedArray[Value], idx: int, argName: string, v: var T) =
  let count = nargs - idx
  v.setLen(count)
  for i in 0 ..< count:
    emToNim(e, args[idx + i], v[i])

when compileOption("threads"):
  var gcInited {.threadVar.}: bool

proc updateStackBottom() {.inline.} =
  when not defined(gcDestructors):
    var a {.volatile.}: int
    nimGC_setStackBottom(cast[pointer](cast[uint](addr a)))
    when compileOption("threads") and not compileOption("tlsEmulation"):
      if not gcInited:
        gcInited = true
        setupForeignThreadGC()

template seqTypeForOpenarrayType[T](t: type openarray[T]): typedesc = seq[T]
template valueTypeForArgType(t: typedesc): typedesc =
  when t is openarray:
    seqTypeForOpenarrayType(t)
  else:
    t

proc typIsVarargs(t: NimNode): bool =
  t.kind == nnkBracketExpr and t.len == 2 and t[0].kind == nnkSym and $t[0] == "varargs"

proc makeCallNimProcWithLispArgs(prc, formalParams, env, nargs, args: NimNode): tuple[parseArgs, call: NimNode] =
  let
    argsVarSection = newNimNode(nnkVarSection)
    parseArgsStmts = newNimNode(nnkStmtList)

  parseArgsStmts.add(argsVarSection)

  let
    origCall = newCall(prc)

  var
    numArgs = 0

  for a in formalParams.arguments:
    let argIdent = newIdentNode("arg" & $a.idx & $a.name)
    let argName = $a.name
    if a.typ.kind == nnkEmpty:
      error("Typeless arguments are not supported: " & $a.name, a.name)
    # XXX: The newCall("type", a.typ) should be just `a.typ` but compilation fails. Nim bug?
    argsVarSection.add(newIdentDefs(argIdent, newCall(bindSym"valueTypeForArgType", newCall("type", a.typ))))
    if typIsVarargs(a.typ):
      parseArgsStmts.add(newCall(bindSym"parseVarargs", env, nargs, args,
                     newLit(a.idx), newLit(argName), argIdent))
    else:
      parseArgsStmts.add(newCall(bindSym"parseArg", env, nargs, args,
                     newLit(a.idx), newLit(argName), argIdent))
    origCall.add(argIdent)
    inc numArgs

  let
    argsLen = newLit(numArgs)
    nameLit = newLit($prc)

  result.parseArgs = quote do:
    if not verifyArgs(`nargs`, `args`, `argsLen`, `nameLit`):
      return Value(nil)
    `parseArgsStmts`

  result.call = origCall

proc getFormalParams(prc: NimNode): NimNode =
  if prc.kind in {nnkProcDef, nnkFuncDef, nnkIteratorDef}:
    result = prc.params
  elif prc.kind == nnkProcTy:
    result = prc[0]
  else:
    # Assume prc is typed
    var impl = getImpl(prc)
    if impl.kind in {nnkProcDef, nnkFuncDef}:
      result = impl.params
    else:
      let ty = getTypeImpl(prc)
      expectKind(ty, nnkProcTy)
      result = ty[0]
  result.expectKind(nnkFormalParams)

proc lispException(e: Env, ex: ref Exception): Value =
  echo "Exception caught: ", ex.msg, ": ", ex.getStackTrace()
  Value(nil)

proc toEmValue[T](e: Env, v: T): Value {.inline.}

proc toEmValueAux[T](v: T): Value =
  # This is a workaround for nim codegen bug. If toEmValue is used directly, C compiler will error.
  when T is void:
    Value(nil)
  else:
    toEmValue(theEnv, v)

macro callNimProcWithLispArgs(prc: typed, e: Env, nargs: int, args: UncheckedArray[Value]): Value =
  let (parseArgs, call) = makeCallNimProcWithLispArgs(prc, prc.getFormalParams, e, nargs, args)
  result = quote do:
    `parseArgs`
    try:
      toEmValueAux(`call`)
    except Exception as ex:
      lispException(`e`, ex)

macro numArgs(prc: typed): untyped =
  var minArity, maxArity: int
  for a in prc.getFormalParams.arguments:
    if typIsVarargs(a.typ):
      maxArity = 99999
    else:
      inc minArity
  if maxArity == 0:
    maxArity = minArity
  result = newLit((minArity, maxArity))

proc nimcallWrapper[T](e: Env, nargs: int, args: UncheckedArray[Value], data: pointer): Value {.cdecl.} =
  theEnv = e
  updateStackBottom()
  proc noinline(e: Env, nargs: int, args: UncheckedArray[Value], data: pointer): Value {.nimcall.} =
    let p = cast[T](data)
    toEmValueAux(callNimProcWithLispArgs(p, e, nargs, args))
  var p {.volatile.}: proc(e: Env, nargs: int, args: UncheckedArray[Value], data: pointer): Value {.nimcall.} = noinline
  p(e, nargs, args, data)

proc nimcallProcToLisp[T](e: Env, p: T): Value =
  let (minArity, maxArity) = numArgs(p)
  e.makeFunction(minArity, maxArity, nimcallWrapper[T], "Nim function", cast[pointer](p))

proc toEmArg*[T](v: T): Value =
  # Don't use this please!
  result = theEnv.toEmValue(v)

proc toEmValue[T](e: Env, v: T): Value =
  when T is SomeInteger:
    e.makeInt(int64(v))
  elif T is SomeFloat:
    e.makeFloat(cdouble(v))
  elif T is string|cstring:
    e.makeString(v, v.len)
  elif T is Value:
    v
  elif T is proc {.nimcall.}:
    e.nimcallProcToLisp(v)
  elif T is proc {.closure.}:
    {.error: "Closures are not supported".}
  elif T is void:
    Value(nil)
  elif T is bool:
    if v:
      e.intern("t")
    else:
      e.intern("NIL")
  else:
    {.error: "Unsupported type to make EmValue " & $T.}

proc funcall*(s: Value, args: varargs[Value, toEmArg]): Value =
  theEnv.funcall(s, args)

proc funcall*(s: cstring, args: varargs[Value, toEmArg]): Value =
  let e = theEnv
  let f = e.intern(s)
  e.funcall(f, args)

proc to*(v: Value, t: typedesc): t =
  when t isnot void:
    emToNim(theEnv, v, result)

proc stringifyNode(n: NimNode): string =
  case n.kind
  of nnkAccQuoted:
    for c in n: result &= $c
  else:
    result = $n

macro makeSweetLispCall*(u: untyped, doStmt: untyped): untyped =
  # Private. Don't use.
  case u.kind
  of nnkIdent, nnkSym, nnkAccQuoted:
    let name = newLit(stringifyNode(u))
    result = quote do:
      intern(theEnv, `name`)
  of nnkCall:
    result = newCall(bindSym"funcall")
    result.add(newLit(stringifyNode(u[0])))
    for i in 1 ..< u.len: result.add(u[i])
    if doStmt.kind != nnkNilLit:
      result.add(doStmt)
  else:
    assert(false)

template enableLispCallSugar*(p: untyped{nkIdent|nkAccQuoted}) =
  template p(u: untyped{nkIdent}): Value {.used.} = makeSweetLispCall(u, nil)
  template p(u: untyped{nkAccQuoted}): Value {.used.} = makeSweetLispCall(u, nil)
  template p(u: untyped{nkCall}): Value {.used.} = makeSweetLispCall(u, nil)
  template p(u: untyped{nkCall}, doStmt: untyped{nkDo}): Value {.used.} = makeSweetLispCall(u, doStmt)

# type Value = module.Value
