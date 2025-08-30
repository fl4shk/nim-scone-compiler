import std/strutils
#import std/sequtils
import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import symType

const
  sconeMaxMacroExpansion*: uint = 1024u

type
  #SconeParserPass* = enum
  #  sconeParserPass0,
  #  sconeParserPassFwdRef,
  SconePass* = enum
    scoPassParse,
    scoPassSymType,
    #scoPassMacroExpansion,
    scoPassSemantic,
    scoPassEmitC,
    limScoPass,

  Scone* = object
    mode*: Mode
    pass*: SconePass
    #macroLim*: uint
    #parserPass*: SconeParserPass
    #ast*: seq[AstNode]
    astRoot*: AstNode
    #ast*: AstNode
    #currAstIdx*: uint64
    symS2d*: seq[seq[Symbol]]
    symNameToIdxTblSeq*: seq[OrderedTable[string, seq[uint64]]]

    savedLexMainSeq*: seq[LexMain]

    lexMain*: LexMain
    #parentExprS2d*: seq[ptr AstNode]
    parentTempSeq*: seq[AstNode]
    
    #line*: string
    #identStrS2d*: seq[seq[string]]
    inputFname*: string
    inp*: string
    outp*: string

#proc globalSymSeq*(
#  self: var Scone,
#): var seq[Symbol] =
#  result = self.symS2d[0]
#
#proc genericSymSeq*(
#  self: var Scone,
#): var seq[Symbol] =
#  result = self.symS2d[1]
#proc funcArgSymSeq*(
#  self: var Scone,
#): var seq[Symbol] =
#  result = self.symS2d[1]
#proc structFieldSymSeq*(
#  self: var Scone,
#): var seq[Symbol] =
#  result = self.symS2d[1]
#
#proc currSymSeq*(
#  self: var Scone,
#): var seq[Symbol] =
#  result = self.symS2d[^1]

#proc mkSymScope*(
#  self: var Scone,
#) =
#  block:
#    var toAdd: seq[Symbol]
#    self.symS2d.add toAdd

proc locInLine*(
  self: var Scone
): var uint64 =
  return self.lexMain.locInLine
proc lineNum*(
  self: var Scone
): var uint64 =
  return self.lexMain.lineNum
proc inpIdx*(
  self: var Scone
): var int =
  return self.lexMain.inpIdx
proc currTok*(
  self: var Scone
): var CurrTok =
  return self.lexMain.currTok

proc stackIdentStrSeq*(
  self: var Scone,
) =
  #self.identStrS2d
  discard


proc stackSavedIlp*(
  self: var Scone
) =
  #echo "stackSavedIlp(): " & $self.currTok
  self.savedLexMainSeq.add self.lexMain
  #self.savedLocInLineSeq.add self.locInLine
  #self.savedLineNumSeq.add self.lineNum
  #self.savedInpIdxSeq.add self.inpIdx
  #self.savedCurrTokSeq.add self.currTok

proc unstackSavedIlp*(
  self: var Scone
) =
  #let oldLenMinus1 = self.savedLocInLineSeq.len() - 1
  let oldLenMinus1 = self.savedLexMainSeq.len() - 1
  #echo "unstackSavedIlp(): before: " & $self.currTok

  self.lexMain = self.savedLexMainSeq[oldLenMinus1]
  #echo "unstackSavedIlp(): after: " & $self.currTok
  self.savedLexMainSeq.setLen(oldLenMinus1)

  #self.locInLine = self.savedLocInLineSeq[oldLenMinus1]
  #self.lineNum = self.savedLineNumSeq[oldLenMinus1]
  #self.inpIdx = self.savedInpIdxSeq[oldLenMinus1]
  #self.currTok = self.savedCurrTokSeq[oldLenMinus1]
  ##echo "unstackSavedIlp(): after: " & $self.currTok

  #self.savedLocInLineSeq.setLen(oldLenMinus1)
  #self.savedLineNumSeq.setLen(oldLenMinus1)
  #self.savedInpIdxSeq.setLen(oldLenMinus1)
  #self.savedCurrTokSeq.setLen(oldLenMinus1)

