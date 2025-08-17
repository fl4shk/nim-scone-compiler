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

proc mkCurrTok(
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
    locInLine*: uint64
    lineNum*: uint64
    #line*: string
    currTok*: CurrTok #(TokKind, Option[string], Option[uint64])
    inputFname: string
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
  proc incrInpIdx(
    self: var Scone,
    amount: int=1,
    doIncrLocInLine: bool=true,
  ) =
    #echo (
    #  (
    #    "incrInpIdx(): "
    #  ) & (
    #    "inpIdx:" & $self.inpIdx & " locInLine:" & $self.locInLine
    #  )
    #)
    if doIncrLocInLine:
      self.locInLine += uint64(amount)
    self.inpIdx += amount

  #if self.inpIdx >= self.inp.len():
  #  self.currTok = mkCurrTok(tokEof, none(string), none(uint64))

  # first eat whitespace
  #echo "eating whitespace"
  while self.inpIdx < self.inp.len():
    if isSpaceAscii(self.inpChar) or self.inpChar == '\n':
      let tempCond = (self.inpChar == '\n')

      if tempCond:
        #echo "increment lineNum: " & $self.lineNum & " " & $self.locInLine
        self.lineNum += 1
        self.locInLine = 1

      self.incrInpIdx(doIncrLocInLine=tempCond)
      #if tempCond:
      #  self.locInLine = 1
    else:
      break


  if self.inpIdx >= self.inp.len():
    self.currTok = mkCurrTok(tokEof, none(string), none(uint64))
    return

  # next determine the kind of token (and if it's valid)
  self.currTok = mkCurrTok(tokBad, none(string), none(uint64))

  var prevLongestSize: (int, Option[int]) = (0, none(int))

  var kwTempStr: array[2, string]
  for idx, (tok, opt) in helperTokKindSeq:
    if opt.isSome:
      #echo "opt.isSome: " & opt.get()
      if self.inpIdx + opt.get.len() < self.inp.len():
        kwTempStr[0] = (
          self.inp[self.inpIdx ..< self.inpIdx + opt.get.len()]
        )
        if kwTempStr[0] == opt.get():
          #echo (
          #  (
          #    "kwTempStr[0] == opt.get(): " 
          #  ) & (
          #    kwTempStr[0] & " " & $kwTempStr[0].len()
          #  )
          #)
          if prevLongestSize[0] < opt.get().len():
            prevLongestSize[0] = opt.get().len()
            prevLongestSize[1] = some(idx)
            kwTempStr[1] = kwTempStr[0]
            self.currTok = mkCurrTok(TokKind(idx), opt, none(uint64))

  if prevLongestSize[1].isSome:
    if kwTempStr[1][0] notin IdentChars:
      #self.inpIdx += prevLongestSize[1].get()
      #echo (
      #  (
      #    "kwTempStr[1][0] notin IdentStartChars: "
      #  ) & (
      #    kwTempStr[1] & " " & $prevLongestSize
      #  )
      #)
      self.incrInpIdx(amount=prevLongestSize[0])
      #echo "debug: " &  self.inp[self.inpIdx .. ^1]
      return

  # check identifiers first
  if self.inpChar in IdentStartChars:
    var tempStr: string
    tempStr.add self.inpChar
    #self.inpIdx += 1
    self.incrInpIdx()

    while self.inpIdx < self.inp.len():
      if self.inpChar in IdentChars:
        tempStr.add self.inpChar
        #self.inpIdx += 1
        self.incrInpIdx()
      else:
        break

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
      self.currTok = mkCurrTok(tokIdent, some(tempStr), none(uint64))
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
    self: var Scone,
    myRangeEnd: Option[char],
  ) =
    #self.inpIdx += 1
    self.incrInpIdx()
    if self.inpIdx >= self.inp.len():
      return

    self.currTok = mkCurrTok(tokUInt64Lit, none(string), some(0u64))

    var tempMul: uint64 = 0u64

    if myRangeEnd.isSome:
      tempMul = uint64(myRangeEnd.get()) - uint64('0') + 1u64
    else: # hexadecimal
      tempMul = 16u64

    #echo "tempMul:" & $tempMul

    var tempU64: uint64 = 0u64
    const hexLowerDigits = {'a' .. 'f'}
    const hexUpperDigits = {'A' .. 'F'}

    var finish: bool = false
    while (
      (
        not finish
      ) and (
        self.inpIdx < self.inp.len()
      )
    ):
      var toSubChar: int32 = int32('0')
      var myAddend: int32 = 0
      if not myRangeEnd.isSome:
        if self.inpChar in hexLowerDigits:
          #echo "have hexLowerDigits: " & self.inpChar
          toSubChar = int32('a')
          myAddend = 10
        elif self.inpChar in hexUpperDigits:
          #echo "have hexUpperDigits: " & self.inpChar
          toSubChar = int32('A')
          myAddend = 10
        elif self.inpChar notin Digits:
          finish = true
      else:
        if self.inpChar notin {'0' .. myRangeEnd.get}:
          finish = true

      let tempInt32 = int32(self.inpChar) - toSubChar + myAddend
      let tempCond = (
        tempInt32 >= 0 and tempInt32 < int32(tempMul)
      )
      if tempCond:
        #echo $tempInt32 & " " & $tempU64
        tempU64 *= tempMul
        tempU64 += tempInt32.uint64()
      else:
        break

      #self.inpIdx += 1
      self.incrInpIdx()

    self.currTok.optU64 = some(tempU64)
    

  const postZeroDigits = {'1' .. '9'}
  if self.inpChar == '0':
    #self.inpIdx += 1

    if self.inpIdx + 1 < self.inp.len():
      #self.inpIdx += 1
      self.incrInpIdx()

      #var tempStr: string

      case self.inpChar:
      of 'x':
        self.handleDigits(myRangeEnd=none(char))
      of 'o':
        self.handleDigits(myRangeEnd=some('7'))
      of 'b':
        self.handleDigits(myRangeEnd=some('1'))
      elif self.inpChar in postZeroDigits:
        self.handleDigits(myRangeEnd=some('9'))
    return
  elif self.inpChar in postZeroDigits:
    self.handleDigits(myRangeEnd=some('9'))

  if self.inpChar == '"':
    discard

template parent(
  child: AstNode
): untyped =
  self.ast[child.parentIdx]
#template currAst(): untyped =
#  self.ast[self.currAstIdx]
#template currParent(): untyped =
#  currAst.parent
  

proc mkAst(
  self: var Scone,
  tok: TokKind,
  litVal: Option[AstLitVal],
  symIdxSeq: seq[uint64],
  parentIdx: uint64,
) =
  let toAdd = AstNode(
    tok: tok,
    lineNum: self.lineNum,
    litVal: litVal,
    symIdxSeq: symIdxSeq,
    parentIdx: parentIdx,
  )
  toAdd.parent.chIdxSeq.add uint64(self.ast.len())
  self.ast.add toAdd

#proc unstackAst(
#  self: var Scone,
#) =
#  #self.currAstIdx = currParent.parentIdx
#  self.currAstIdx = currAst.parentIdx
proc expect(
  self: var Scone,
  tok: TokKind,
) =
  doAssert(
    tok == self.currTok.tok,
    (
      (
        "error: "
      ) & (
        "expected:" & $tok & ", but have " & $self.currTok.tok & " "
      ) & (
        "at this location: " & self.inputFname & ":" & $self.lineNum
      )
    )
  )

proc doCompileModeOneFile(
  self: var Scone
) =
  while self.inpIdx < self.inp.len():
    #echo (
    #  (
    #    "before: " & $self.inpIdx & " "
    #  ) & (
    #     $self.lineNum & ":" & $self.locInLine
    #  )
    #)
    self.lex()
    #echo (
    #  (
    #    "after: " & $self.inpIdx & " "
    #  ) & (
    #     $self.lineNum & ":" & $self.locInLine
    #  )
    #)
    echo $self.currTok
  #echo $self.currTok
  #echo self.
  #discard

proc mkScone*(
  myMode: Mode,
  inputFname: string,
): Scone =
  result.mode = myMode

  result.ast.add AstNode(
    tok: tokInternalAstStart,
    lineNum: 0.uint64,
    litVal: none(AstLitVal),
  )
  result.locInLine = 1
  result.lineNum = 1
  result.inputFname = inputFname
  result.inp = readFile(filename=inputFname)
  result.inpIdx = 0
  result.outp = ""

  case myMode:
  of mdOneFile:
    result.doCompileModeOneFile()
