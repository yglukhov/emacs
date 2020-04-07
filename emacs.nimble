# Package

version       = "0.1.0"
author        = "Yuriy Glukhov"
description   = "Write emacs modules in nim"
license       = "MIT"


# Dependencies

requires "nim >= 1.2.0"

task runTest, "Compile the sample and run emacs":
  exec("nim c --app:lib --nomain -o:mymod.so --passC:-g tests/test1.nim")
  exec("emacs")
