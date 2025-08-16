##import std/sequtils
#import std/algorithm
#import std/deques
import std/cmdline
#import std/math
#import miscMath
#import mode
#import std/options

import dataStructures
import doCompile

#proc isSpace(c: char): bool =
#  result = c in Whitespace

let paramsSeq = commandLineParams()

proc usage() =
  echo "Usage 0: nimSconeCompiler --one-file input.scone output.c"
  # TODO: add more modes of compilation later!
  doAssert(false)

var inputFname: string
var outputFname: string
var myMode: Mode


if paramsSeq.len() == 3:
  inputFname = paramsSeq[1]
  outputFname = paramsSeq[2]
  case paramsSeq[0]:
  of "--one-file":
    myMode = mdOneFile
  else:
    usage()
else:
  usage()

let scone = mkScone(
  myMode=myMode,
  inputFname=inputFname,
)
writeFile(filename=outputFname, content=scone.outp)
