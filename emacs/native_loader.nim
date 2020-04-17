import std/[dynlib, strutils, os, osproc, sha1, tables]
import ../emacs, ./sugar, ./module, ./utils

implementModule(Plugin_is_GPL_compatible)

var loadedSos: Table[string, LibHandle]

proc unloadFile(srcPath: string): bool =
  var h: LibHandle
  if loadedSos.take(srcPath, h) and not h.isNil:
    unloadLib(h)
    result = true

proc libPathWithSrcPath(path: string): string =
  result = path
  if path.endsWith(".nim"):
    let fn = splitFile(path).name
    let ext = when defined(macosx):
      ".dylib"
    elif defined(windows):
      ".dll"
    else:
      ".so"
    result = getTempDir() / $secureHash(path) & "_" & fn & ext

proc compileFile(srcPath: string): bool =
  if srcPath.endsWith(".nim"):
    let target = libPathWithSrcPath(srcPath)
    let (o, r) = execCmdEx("nim c --app:lib --nomain -o:" & target & " --passC:-g " & srcPath)
    if r == 0:
      result = true
    else:
      echo "Failed to compile ", srcPath
      echo o
  else:
    result = true

proc loadFile(srcPath: string): bool =
  discard unloadFile(srcPath)

  if compileFile(srcPath):
    let h = loadLib(libPathWithSrcPath(srcPath))
    if not h.isNil:
      let gplCompat = h.symAddr("plugin_is_GPL_compatible")
      if not gplCompat.isNil:
        let initf = cast[proc(r: Runtime): cint {.cdecl.}](h.symAddr("emacs_module_init"))
        if not initf.isNil:
          var r: typeof(default(Runtime)[])
          r.size = sizeof(r)
          r.getEnvironment = proc(r: Runtime): Env {.cdecl.} =
            theEnv
          if initf(addr r) == 0:
            result = true
          else:
            echo "Init function failed: ", srcPath
        else:
          echo "No init function in plugin: ", srcPath
      else:
        echo "Plugin not GPL compatible: ", srcPath

      if result:
        echo "Loaded ", srcPath
        loadedSos[srcPath] = h
      else:
        unloadLib(h)
  else:
    echo "Failed to compile: ", srcPath

proc main() =
  discard ~fset(~`nim-native/load`) do(args: varargs[Value]) -> bool:
    let path = expandTilde(args[0].to(string))
    loadFile(path)

  proc reloadAll() =
    var s: seq[string]
    for k, _ in loadedSos: s.add(k)
    for k in s: discard loadFile(k)

  discard ~fset(~`nim-native/reload-all`, makeInteractiveFunction(reloadAll))

  discard ~fset(~`nim-native/unload`) do(path: string) -> bool:
    let path = expandTilde(path)
    unloadFile(path)

  discard ~`global-set-key`(~kbd("C-c r r"), ~`nim-native/reload-all`)

  discard ~provide(~`nim-native`)

main()
