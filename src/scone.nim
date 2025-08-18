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
    savedCurrTokSeq*: seq[CurrTok]

    locInLine*: uint64
    lineNum*: uint64
    inpIdx*: int
    currTok*: CurrTok
    #line*: string
    currIdentStrSeq*: seq[string]
    inputFname*: string
    inp*: string
    outp*: string


proc stackSavedIlp*(
  self: var Scone
) =
  #echo "stackSavedIlp(): " & $self.currTok
  self.savedLocInLineSeq.add self.locInLine
  self.savedLineNumSeq.add self.lineNum
  self.savedInpIdxSeq.add self.inpIdx
  self.savedCurrTokSeq.add self.currTok

proc unstackSavedIlp*(
  self: var Scone
) =
  let oldLenMinus1 = self.savedLocInLineSeq.len() - 1
  #echo "unstackSavedIlp(): before: " & $self.currTok

  self.locInLine = self.savedLocInLineSeq[oldLenMinus1]
  self.lineNum = self.savedLineNumSeq[oldLenMinus1]
  self.inpIdx = self.savedInpIdxSeq[oldLenMinus1]
  self.currTok = self.savedCurrTokSeq[oldLenMinus1]
  #echo "unstackSavedIlp(): after: " & $self.currTok

  self.savedLocInLineSeq.setLen(oldLenMinus1)
  self.savedLineNumSeq.setLen(oldLenMinus1)
  self.savedInpIdxSeq.setLen(oldLenMinus1)
  self.savedCurrTokSeq.setLen(oldLenMinus1)

