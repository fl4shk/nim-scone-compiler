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

  var prevLongestSize: (int, Option[int]) = (0, none(int))

  for idx, (tok, opt) in helperTokKindSeq:
    if opt.isSome:
      if self.inpIdx + opt.get.len() < self.inp.len():
        let tempStr = self.inp[self.inpIdx .. self.inpIdx + opt.get.len()]
        if tempStr == opt.get():
          if prevLongestSize[0] < opt.get().len():
            prevLongestSize[0] = opt.get().len()
            prevLongestSize[1] = some(idx)
            echo "`lex()`: Here is my string: " & tempStr
            self.currTok = (TokKind(idx), opt, none(uint64))

  # check identifiers first
  if self.inpChar in IdentStartChars:
    var tempStr: string
    tempStr.add self.inpChar
    self.inpIdx += 1

    while self.inpIdx < self.inp.len():
      if self.inpChar in IdentChars:
        tempStr.add self.inpChar
        self.inpIdx += 1
      else:
        break;
    var tempCond: bool = false
    if prevLongestSize[1].isSome():
      let tempOpt = (
        helperTokKindSeq[prevLongestSize[1].get()][1]
      )
      assert(
        tempOpt.isSome()
      )
      if tempStr == tempOpt.get():
        tempCond = true
    if not tempCond:
      self.currTok = (tokIdent, some(tempStr), none(uint64))
    return

  #proc handleDigits(
  #  mySet: set[char]
  #) = 
  #  while self.inpIdx < self.inp.len() :
  #    if self.inpChar in mySet:
  #      discard
  #    else:
  #      return
  proc handleDigits(
    myRangeEnd: Option[char]
  ) =
    self.inpIdx += 1
    if self.inpIdx >= self.inp.len():
      return

    self.currTok = (tokUInt64Lit, none(string), some(0u64))

    var tempMul: uint64 = 0u64

    if myRangeEnd.isSome:
      tempMul = uint64(myRangeEnd.get()) + 1u64
    else: # hexadecimal
      tempMul = 16u64

    var tempU64: uint64 = 0u64
    const hexLowerDigits = {'a' .. 'f'}
    const hexUpperDigits = {'A' .. 'F'}

    while self.inpIdx < self.inp.len():
      var toSubChar: int32 = int32('0')
      if not myRangeEnd.isSome:
        if self.inpChar in hexLowerDigits:
          toSubChar = int32('a')
        elif self.inpChar in hexUpperDigits:
          toSubChar = int32('A')
      let tempInt32 = int32(self.inpChar) - toSubChar
      if tempInt32 >= 0 and tempInt32 < int32(tempMul):
        tempU64 *= tempMul
        tempU64 += tempInt32.uint64()

      self.inpIdx += 1

    self.currTok[2] = some(tempU64)
    

  const postZeroDigits = {'1' .. '9'}
  if self.inpChar == '0':
    #self.inpIdx += 1


    if self.inpIdx + 1 < self.inp.len():
      self.inpIdx += 1

      var tempStr: string

      case self.inpChar:
      of 'x':
        handleDigits(myRangeEnd=none(char))
      of 'o':
        handleDigits(myRangeEnd=some('7'))
      of 'b':
        handleDigits(myRangeEnd=some('1'))
      elif self.inpChar in postZeroDigits:
        handleDigits(myRangeEnd=some('9'))
    return
  elif self.inpChar in postZeroDigits:
    handleDigits(myRangeEnd=some('9'))

    
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
