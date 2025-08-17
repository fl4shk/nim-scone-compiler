import std/strutils
#import std/sequtils
import std/tables
import std/sets
import std/options

import dataStructures

type
  CurrTok* = object
    tok*: TokKind
    optStr*: Option[string]
    optU64*: Option[uint64]

proc mkCurrTok*(
  tok: TokKind,
  optStr: Option[string],
  optU64: Option[uint64],
): CurrTok = CurrTok(
  tok: tok,
  optStr: optStr,
  optU64: optU64
)

type
  Scone* = object
    mode*: Mode
    ast*: seq[AstNode]
    currAstIdx*: uint64
    symSeq*: seq[Symbol]
    symNameToIdxTbl*: OrderedTable[string, uint64]

    savedLocInLineSeq*: seq[uint64]
    savedLineNumSeq*: seq[uint64]
    savedInpIdxSeq*: seq[int]

    locInLine*: uint64
    lineNum*: uint64
    inpIdx*: int
    #line*: string
    currTok*: CurrTok
    inputFname*: string
    inp*: string
    outp*: string


proc stackSavedIlp*(
  self: var Scone
) =
  self.savedLocInLineSeq.add self.locInLine
  self.savedLineNumSeq.add self.lineNum
  self.savedInpIdxSeq.add self.inpIdx

proc unstackSavedIlp*(
  self: var Scone
) =
  let oldLenMinus1 = self.savedLocInLineSeq.len() - 1
  self.savedLocInLineSeq.setLen(oldLenMinus1)
  self.savedLineNumSeq.setLen(oldLenMinus1)
  self.savedInpIdxSeq.setLen(oldLenMinus1)

