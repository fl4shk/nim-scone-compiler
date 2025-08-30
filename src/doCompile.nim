#import std/strutils
#import std/sequtils
import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import scone
import passParse
import passSymType
import passEmitC


proc doCompileModeOneFile(
  self: var Scone
) =
  #while self.inpIdx < self.inp.len():
  #  #echo (
  #  #  (
  #  #    "before: " & $self.inpIdx & " "
  #  #  ) & (
  #  #     $self.lineNum & ":" & $self.locInLine
  #  #  )
  #  #)
  #  self.lex()
  #  #echo (
  #  #  (
  #  #    "after: " & $self.inpIdx & " "
  #  #  ) & (
  #  #     $self.lineNum & ":" & $self.locInLine
  #  #  )
  #  #)
  #  echo $self.currTok
  #echo $self.currTok

  #self.parseModule()
  for passUint in 0u ..< uint(limScoPass):
    self.pass = SconePass(passUint)
    case self.pass:
    of scoPassParse:
      self.doPassParse()
      echo $self.astRoot
    of scoPassSymType:
      self.doPassSymType()
    of scoPassEmitC:
      self.doPassEmitC()
    else:
      doAssert(
        false,
        "eek! " & $self.pass
      )

proc mkScone*(
  myMode: Mode,
  inputFname: string,
): Scone =
  result.mode = myMode

  result.lexMain.locInLine = 1
  result.lexMain.lineNum = 1
  result.lexMain.inpIdx = 0
  result.astRoot = AstNode(
    #tok: tokInternalAstStart,
    #lineNum: 0.uint64,
    lexMain: result.lexMain,
    #litVal: none(AstLitVal),
    #parent: nil,
    kind: astSrcFile,
    #module: nil,
    mySrcFile: AstSrcFile(
      module: nil,
    )
  )
  #result.ast = result.astRoot
  #result.ast.parent = nil

  #result.ast.module = nil
  #result.srcFileVal.

  #var identStrSeq: seq[string]
  #result.identStrS2d.add identStrSeq

  result.inputFname = inputFname
  result.inp = readFile(filename=inputFname)
  result.outp = ""

  case myMode:
  of mdOneFile:
    result.doCompileModeOneFile()
