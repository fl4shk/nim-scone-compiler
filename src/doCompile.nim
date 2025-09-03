#import std/strutils
#import std/sequtils
#import std/tables
#import std/sets
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
  for pass in SconePass(0u) ..< limScoPass:
    self.pass = pass
    case self.pass:
    of scoPassParse:
      self.doPassParse()
      #echo $self.astRoot
    of scoPassSymType:
      self.doPassSymType()
      #echo $self.symS2d
      #echo ""
      #echo ""
      #for idx in 0 ..< self.symS2d.len():
      #  let tempSeq = addr self.symS2d[idx]
      #  if tempSeq[].len() > 0:
      #    echo "seq " & $idx & ":"
      #    echo "  #----"
      #  for jdx in 0 ..< tempSeq[].len():
      #    
      #    #echo $self.symS2d[idx]
      #    let sym = addr tempSeq[][jdx]
      #    let typeInfo = addr self.typeInfoS2d[idx][sym[].typeInfoIdx]
      #    #echo $(sym[], typeInfo[])
      #    echo "  sym " & $jdx & ": " & $sym[]
      #    echo "  typeInfo: " & $typeInfo[]
      #    echo "  #----"
      #  if tempSeq[].len() > 0:
      #    echo "#--------"
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
