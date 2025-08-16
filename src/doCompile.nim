import std/strutils
import std/sequtils
import std/tables
import std/sets

import dataStructures


type
  Scone* = object
    mode*: Mode
    ast*: seq[AstNode]
    currAstIdx*: uint64
    symSeq*: seq[Symbol]
    symNameToIdxTbl*: OrderedTable[string, uint64]
    lineNum*: uint64
    #line*: string
    inp*: string
    inpIdx*: int
    outp*: string

proc inpChar(
  self: var Scone
): char =
  result = self.inp[self.inpIdx]


proc lex(
  self: var Scone,
) =
  # first eat whitespace
  while self.inpIdx < self.inp.len():
    if isSpaceAscii(self.inpChar):
      self.inpIdx += 1

  # next determine the kind of token (if it's valid)

proc mkAst(
  self: var Scone,
  tok: TokKind,
  strData: string,
) =
  self.ast.add AstNode(
    tok: tok,
    lineNum: self.lineNum,
  )

proc unstackAst(
  self: var Scone,
) =
  let myNode = addr self.ast[self.currAstIdx]
  self.currAstIdx = myNode[].parentIdx

proc doCompileModeOneFile(
  self: var Scone
) =
  self.lex()
  discard

proc mkScone*(
  myMode: Mode,
  inputFname: string,
): Scone =
  result.mode = myMode

  result.ast.add AstNode(
    tok: tokInternalAstStart,
    lineNum: 0.uint64,
    strData: ""
  )
  result.lineNum = 1
  result.inp = readFile(filename=inputFname)
  result.outp = ""

  case myMode:
  of mdOneFile:
    result.doCompileModeOneFile()
