import std/strutils
import std/sequtils
import std/tables
import std/sets
import std/options

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
    currTok*: (TokKind, Option[string], Option[uint64])
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

  # next determine the kind of token (and if it's valid)
  self.currTok = (tokBad, none(string), none(uint64))

  if self.inpIdx >= self.inp.len():
    return

  for idx, (tok, opt) in helperTokKindSeq:
    if opt.isSome:
      if self.inpIdx + opt.get.len() < self.inp.len():
        let tempStr = self.inp[self.inpIdx .. self.inpIdx + opt.get.len()]
        if tempStr == opt.get():
          echo "Here is my string: " & tempStr
          self.currTok = (TokKind(idx), opt, none(uint64))
        

  # check identifiers first
  if self.inpChar in IdentStartChars:
    self.currTok[0] = tokIdent
    var tempStr: string
    tempStr.add self.inpChar
    self.inpIdx += 1

    while self.inpIdx < self.inp.len():
      if self.inpChar in IdentChars:
        tempStr.add self.inpChar
        self.inpIdx += 1
      else:
        break;
    return

  proc handleDigits(
    mySet: set[char]
  ) = 
    while self.inpIdx < self.inp.len() :
      if self.inpChar in mySet:
        discard
      else:
        return

  if self.inpChar == '0':
    #self.inpIdx += 1

    self.currTok = (tokUInt64Lit, none(string), some(uint64(0)))

    if self.inpIdx + 1 < self.inp.len():
      self.inpIdx += 1

      var tempStr: string

      case self.inpChar:
      of 'x':
        handleDigits(HexDigits)
      #of 'o':
      #  handleDigits({'0' .. '7'})
      of 'b':
        handleDigits({'0' .. '1'})
        #let prevInpIdx = self.inpIdx - 1
        #self.inpIdx -= 1
        #self.currTok = (
        #  tokIntLit, none(string), some(parseBiggestUInt())
        #)
      elif self.inpChar in {'1' .. '9'}:
        handleDigits(Digits)
    return
  elif self.inpChar in {'1' .. '9'}:
    handleDigits(Digits)

    
  #for idx in 0 ..< 


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
  #self.lex()
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
