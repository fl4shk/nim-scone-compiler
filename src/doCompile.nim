import std/strutils
import std/tables
import std/sets

import dataStructures


type
  Scone* = object
    ast*: seq[AstNode]
    symSeq*: seq[Symbol]
    outp*: string


proc mkScone*(
  myMode: Mode,
): Scone =
  case myMode:
  of mdOneFile:
    discard

  result.ast.add AstNode(
    tok: tokInternalAstStart
  )
