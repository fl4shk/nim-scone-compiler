import std/sets
import std/options
import std/strutils

import nonAstDataStructures
import scone

proc inpChar(
  self: var Scone
): char =
  result = self.inp[self.inpIdx]

proc lexInternal*(
  self: var Scone,
)

proc lex*(
  self: var Scone,
) =
  self.lexInternal()
  #echo "lex result: " & $self.lexMain
  
proc lexInternal*(
  self: var Scone,
) =
  proc incrInpIdx(
    self: var Scone,
    amount: int=1,
    doIncrLocInLine: bool=true,
  ) =
    #doAssert(
    #  self.inpChar != '2'
    #)
    #echo (
    #  (
    #    "incrInpIdx(): "
    #  ) & (
    #    "inpIdx:" & $self.inpIdx & " locInLine:" & $self.locInLine
    #  ) & (
    #    "   inpChar:" & self.inpChar & " amount:" & $amount
    #  )
    #)
    if doIncrLocInLine:
      self.locInLine += uint64(amount)
    self.inpIdx += amount

  #if self.inpIdx >= self.inp.len():
  #  self.currTok = mkCurrTok(tokEof, none(string), none(uint64))

  # first eat whitespace
  #echo "eating whitespace"
  proc eatWhitespace(
    self: var Scone,
  ): bool =
    result = false
    while self.inpIdx < self.inp.len():
      if isSpaceAscii(self.inpChar) or self.inpChar == '\n':
        let tempCond = (self.inpChar == '\n')

        if tempCond:
          #echo "increment lineNum: " & $self.lineNum & " " & $self.locInLine
          self.lineNum += 1
          self.locInLine() = 0

        self.incrInpIdx(doIncrLocInLine=true)
        #if tempCond:
        #  self.locInLine = 1
      else:
        break
    if self.inpIdx >= self.inp.len():
      return true

  while true: #not self.eatWhitespace():
    if self.eatWhitespace():
      break
    if self.inpChar == '#':
      self.incrInpIdx(doIncrLocInLine=true)
      var tempCond = false
      while (self.inpIdx < self.inp.len()) and not tempCond:
        if self.inpChar == '\n':
          tempCond = true
        self.incrInpIdx(doIncrLocInLine=false)
      self.lineNum += 1
      self.locInLine() = 1
    else:
      break

    if self.inpIdx >= self.inp.len():
      break

  if self.inpIdx >= self.inp.len():
    self.currTok() = mkCurrTok(tokEof, none(string), none(uint64))
    return

  # next determine the kind of token (and if it's valid)
  self.currTok() = mkCurrTok(tokBad, none(string), none(uint64))

  var prevLongestSize: (int, Option[int]) = (0, none(int))

  var kwTempStr: array[2, string]
  for idx, (tok, opt, dontCare0, dontCare1) in helperTokKindSeq:
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
            self.currTok() = mkCurrTok(TokKind(idx), opt, none(uint64))

  if prevLongestSize[1].isSome:
    if kwTempStr[1][0] notin IdentChars:
      #self.inpIdx += prevLongestSize[1].get()
      #echo (
      #  (
      #    "kwTempStr[1][0] notin IdentStartChars: "
      #  ) & (
      #    "\"" & kwTempStr[1] & "\" " & $prevLongestSize
      #  )
      #)
      self.incrInpIdx(amount=prevLongestSize[0])
      #echo "kwTempStr[1][0] debug: " & self.inpChar
      #echo "debug: " & self.inp[self.inpIdx .. ^1]
      return

  # check identifiers first
  if self.inpChar in IdentStartChars:
    #echo "self.inpChar in IdentStartChars" 
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
      self.currTok() = (
        mkCurrTok(tokIdent, some(tempStr), none(uint64))
      )
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
    doInitialIncr: bool=true,
  ) =
    #self.inpIdx += 1
    if doInitialIncr:
      self.incrInpIdx()
    if self.inpIdx >= self.inp.len():
      return

    self.currTok() = (
      mkCurrTok(tokU64Lit, none(string), some(0u64))
    )

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
      toSubChar = int32('0')
      myAddend = 0
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
      if myRangeEnd.isSome:
        discard
        #echo (
        #  (
        #    "debug: tempInt32, tempCond: "
        #  ) & (
        #    $(self.inpChar, tempInt32, tempCond)
        #  )
        #)
      if tempCond:
        if myRangeEnd.isSome:
          #echo "before: " & $tempInt32.uint64() & " " & $tempU64
          discard
        tempU64 *= tempMul
        tempU64 += tempInt32.uint64()
        if myRangeEnd.isSome:
          #echo "after: " & $tempInt32 & " " & $tempU64
          discard
      else:
        if myRangeEnd.isSome:
          #echo "break: " & $tempInt32 & " " & $tempU64
          discard
        #self.incrInpIdx()
        break

      #self.inpIdx += 1
      #if not finish:
      self.incrInpIdx()

    self.currTok.optU64 = some(tempU64)
    

  const postZeroDigits = {'1' .. '9'}
  if self.inpChar == '0':
    self.currTok() = (
      mkCurrTok(tokU64Lit, none(string), some(0u64))
    )
    #self.inpIdx += 1
    #self.currTok.optU64 = some(0u64)

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
        self.handleDigits(myRangeEnd=some('9'), doInitialIncr=false)
    #echo "orig: self.inpChar == : '0': " & $self.currTok
    return
  elif self.inpChar in postZeroDigits:
    self.currTok() = (
      mkCurrTok(tokU64Lit, none(string), some(0u64))
    )
    self.handleDigits(myRangeEnd=some('9'), doInitialIncr=false)
    #echo "self.inpChar in postZeroDigits: " & $self.currTok

  #echo "post handleDigits(): \"" & self.inpChar & "\""

  if self.inpChar == '"':
    discard

proc locMsg*(
  self: var Scone
): string =
  result = (
    (
      "at this location: " & self.inputFname & ":"
    ) & (
      $self.lineNum & "," & $self.locInLine
    )
  )

proc expect*(
  self: var Scone,
  tokSet: HashSet[TokKind],
) =
  doAssert(
    #tok == self.currTok.tok,
    self.currTok.tok in tokSet,
    (
      (
        "error: "
      ) & (
        "expected:" & $tokSet & ", but have " 
      ) & (
        $self.currTok & " "
      ) & (
        self.locMsg()
      )
    )
  )

proc lexAndExpect*(
  self: var Scone,
  tokSet: HashSet[TokKind],
) =
  self.lex()
  self.expect(tokSet)

proc expect*(
  self: var Scone,
  tok: TokKind,
) =
  self.expect(toHashSet([tok]))

proc lexAndExpect*(
  self: var Scone,
  tok: TokKind,
) =
  self.lex()
  self.expect(tok)

proc lexAndCheck*(
  self: var Scone,
  chk: bool,
  tokSet: HashSet[TokKind],
  #someSpp: SelParseProc,
): Option[TokKind] =
  result = none(TokKind)
  #if chk:
  self.stackSavedIlp()
  self.lex()
  #if not someSpp(self=self, chk=chk):
  #  result = true
  if self.currTok.tok in tokSet:
    result = some(self.currTok.tok)
  self.unstackSavedIlp()
  if not chk:
  #else:
    self.lexAndExpect(tokSet)

proc lexAndCheck*(
  self: var Scone,
  chk: bool,
  tok: TokKind,
): Option[TokKind] =
  result = self.lexAndCheck(chk=chk, tokSet=toHashSet([tok]))
proc lexAndCheck*(
  self: var Scone,
  chk: bool,
  tokArr: openArray[TokKind],
): Option[TokKind] =
  result = self.lexAndCheck(chk=chk, tokSet=toHashSet(tokArr))
