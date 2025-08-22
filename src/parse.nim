import std/tables
import std/sets
import std/options

import dataStructures
import scone
import lex

template parent(
  child: AstNode
): untyped =
  self.ast[child.parentIdx]
#template currAst(): untyped =
#  self.ast[self.currAstIdx]
#template currParent(): untyped =
#  currAst.parent
  

proc mkAst*(
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
type
  SppResult* = object
    tokSet*: HashSet[TokKind]
    foundTok*: Option[TokKind]
    foundTok1*: Option[TokKind]
  #SppResult* = SppResult

type
  SelParseProc = proc(
    self: var Scone,
    chk: bool,
  ): SppResult {.closure.}

#template SelParseProc(): untyped = 
#  proc(self: var Scone, chk: bool): bool
#template sppSeq(
#  someProc: untyped
#): untyped =
#  SelParseProc(someProc)

template spp(
  someProc: untyped
): untyped =
  SelParseProc someProc
template sppSeq(
  someSppSeq: untyped
): untyped =
  var hiddenOutpSeq: seq[SelParseProc]
  for mySpp in someSppSeq:
    hiddenOutpSeq.add spp(mySpp)
  hiddenOutpSeq

template doChkTokSet(
  argTokSet: untyped,
): untyped =
  #result = none(SppResultMain)
  #result = SppResult(foundTok: none(TokKind))
  result.tokSet = argTokSet
  #result.foundTok = none(TokKind)
  #let hiddenMyTok = 
  result.foundTok = (
    self.lexAndCheck(chk=chk, argTokSet)
  )
  if chk:
    #if result.foundTok.isSome:
    #  #return some(SppResultMain(
    #  #  tokSet: argTokSet,
    #  #  foundTok: hiddenMyTok.get(),
    #  #))
    #  #return SppResult(
    #  #  tokSet: argTokSet,
    #  #  foundTok: hiddenMyTok,
    #  #)
    #  result.foundTok = hiddenMyTok
    #else:
    #  #return none(SppResultMain)
    #  return
    return
  ##hiddenMyTok
  result

template doChkTok(
  argTok: untyped,
): untyped =
  let hiddenArgTokSet = toHashSet([argTok])
  doChkTokSet(
    hiddenArgTokSet
  )

template doChkSpp(
  selProc: untyped,
): untyped =
  #result = none(SppResultMain)
  #result = SppResult(foundTok: none(TokKind))
  result.foundTok = none(TokKind)
  let hiddenMySppRet = selProc(self=self, chk=chk)
  if chk:
    return hiddenMySppRet
  result = hiddenMySppRet
  result

template doChkSelParse(
  someSppSeq: untyped,
  optLastTokSet: untyped,
): untyped = 
  result.foundTok = none(TokKind)
  #echo "doChkSelParse(): begin: chk:" & $chk
  let hiddenMySpp = self.selParse(
    chk=false,
    #sppSeq @[
    #  parseIdent,
    #  subParseParenExpr,
    #],
    someSppSeq
  )
  #echo "doChkSelParse(): post selParse: " & $self.lexMain
  if chk:
    if hiddenMySpp[1].foundTok.isSome:
      #echo "doChkSelParse(): chk==true, returning: " & $hiddenMySpp
      return hiddenMySpp[1]
  elif hiddenMySpp[1].foundTok.isSome:
    discard hiddenMySpp[0](self=self, chk=chk)
  else:
    # this will be an error
    #echo "this will be an error"
    if optLastTokSet.isSome:
      self.lexAndExpect(
        tokSet=hiddenMySpp[1].tokSet.union(optLastTokSet.get())
      )
    else:
      self.lexAndExpect(
        tokSet=hiddenMySpp[1].tokSet
      )
    doAssert(
      # just in case, also have this error handler
      false,
      "eek! " & $self.currTok
    )
  hiddenMySpp

proc optParse(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
): SppResult =
  #result = SppResult(foundTok: none(TokKind))
  #result.foundTok = none(TokKind)
  #self.stackSavedIlp()
  result = selProc(self=self, chk=true)
  if result.foundTok.isSome:
    if not chk:
      #self.unstackSavedIlp()
      #echo "optParse(): foundTok.isSome: not chk: returning"
      return selProc(self=self, chk=false)
  #echo "optParse(): chk: returning"
  #self.unstackSavedIlp()
      
  #let sppRet = selProc(self=self, chk=true)
  #if sppRet.foundTok.isSome:
  #  if chk:
  #    result = sppRet
  #  else: # if not chk
  #    self.unstackSavedIlp()
  #    return selProc(self=self, chk=false)
  #self.unstackSavedIlp()

proc optParseThenExpectTokSet(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
  postTokSet: HashSet[TokKind],
): SppResult =
  #result = self.optParse(
  #  chk=true,
  #  selProc=selProc,
  #)
  #if not result.foundTok.isSome:
  #  result.tokSet = result.tokSet.union(postTokSet)
  #  self.lexAndExpect(result.tokSet)
  #else:
  #  self.lexAndExpect(postTokSet)

  #result = self.optParse(
  #  chk=true,
  #  selProc=selProc,
  #)
  result = selProc(self=self, chk=true)
  if chk:
    #let postResult = postSelProc(self=self, chk=true)
    let postFoundTok = self.lexAndCheck(chk=true, tokSet=postTokSet)
    if result.foundTok.isSome:
      discard
    elif postFoundTok.isSome:
      result.foundTok = postFoundTok
    result.tokSet = result.tokSet.union(postTokSet)
  else: # if not chk:
    if not result.foundTok.isSome:
      let postFoundTok = self.lexAndCheck(chk=true, tokSet=postTokSet)
      if not postFoundTok.isSome:
        # this will be an error
        self.lexAndExpect(tokSet=result.tokSet.union(postTokSet))
      else: # if postFoundTok.isSome:
        self.lexAndExpect(tokSet=postTokSet)
    else: # if result.foundTok.isSome:
      result = selProc(self=self, chk=false)
      self.lexAndExpect(tokSet=postTokSet)

proc optParseThenExpectTokSeq(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
  postTokSeq: seq[TokKind],
): SppResult =
  result = self.optParseThenExpectTokSet(
    chk=chk,
    selProc=selProc,
    postTokSet=toHashSet(postTokSeq),
  )

proc optParseThenExpectSpp(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
  postSelProc: SelParseProc,
): SppResult = 
  #result = self.optParse(
  #  chk=true,
  #  selProc=selProc,
  #)
  #let temp = postSelProc(
  #  self=self,
  #  chk=true
  #)
  ##result.tokSet = result.tokSet.union(temp.tokSet)
  #result = self.optParseThenExpectTokSet(
  #  chk=chk,
  #  selProc=selProc,
  #  postTokSet=temp.tokSet,
  #)

  #result = self.optParse(
  #  chk=true,
  #  selProc=selProc,
  #)
  result = selProc(self=self, chk=true)
  if chk:
    let postResult = postSelProc(self=self, chk=true)
    if result.foundTok.isSome:
      discard
    elif postResult.foundTok.isSome:
      result.foundTok = postResult.foundTok
    result.tokSet = result.tokSet.union(postResult.tokSet)
  else: # if not chk
    #echo "not chk"
    if not result.foundTok.isSome:
      #echo "not result.foundTok.isSome: " & $result
      #let postFoundTok = self.lexAndCheck(chk=true, tokSet=postTokSet)
      let postResult = postSelProc(self=self, chk=true)
      #echo "postResult: " & $postResult
      if not postResult.foundTok.isSome:
        #echo "postResult.foundTok.isSome == false"
        # this will be an error
        self.lexAndExpect(
          tokSet=result.tokSet.union(postResult.tokSet),
        )
      else: # if postFoundTok.isSome:
        #echo (
        #  (
        #    "postResult.foundTok.isSome == true: "
        #  ) & (
        #    #$postResult.foundTok.get()
        #    $postResult & "  " & $self.lexMain
        #  )
        #)
        #self.lexAndExpect(tokSet=postTokSet)
        result = postSelProc(self=self, chk=false)
    else: # if result.foundTok.isSome:
      result = selProc(self=self, chk=false)
      discard postSelProc(self=self, chk=false)
      #self.lexAndExpect(tokSet=postTokSet)
  #echo "following: " & $result & " " & $self.currTok
  


proc selParse(
  self: var Scone,
  chk: bool,
  selProcSet: HashSet[SelParseProc],
): (SelParseProc, SppResult) =
  #for myProc in selProcSet:
  #  if myProc[](self, true):
  #    return myProc
  #for idx in 0 ..< selProcSet.len():
  result[1].foundTok = none(TokKind)
  for selProc in selProcSet:
    self.stackSavedIlp()
    let sppRet = selProc(self=self, chk=true)
    result[1].tokSet = result[1].tokSet.union(sppRet.tokSet)

    if sppRet.foundTok.isSome:
      result[0] = selProc
      if chk:
        result[1].foundTok = sppRet.foundTok
      else:
        self.unstackSavedIlp()
        return (selProc, sppRet)
        
    self.unstackSavedIlp()
    
  #echo "selParse(): returning `none`"

proc selParse(
  self: var Scone,
  chk: bool,
  selProcSeq: seq[SelParseProc],
): (SelParseProc, SppResult) =
  result = self.selParse(chk=chk, selProcSet=toHashSet(selProcSeq))
  #self.stackSavedIlp()
  #self.lex()
  #echo "selParse test: " & $self.currTok
  #self.unstackSavedIlp()

proc selParse(
  self: var Scone,
  selTokSet: HashSet[TokKind],
): Option[TokKind] =
  for selTok in selTokSet:
    self.stackSavedIlp()
    let tok = self.lexAndCheck(chk=true, tok=selTok)
    if tok.isSome:
      self.unstackSavedIlp()
      return tok
    self.unstackSavedIlp()
  return none(TokKind)

proc selParse(
  self: var Scone,
  selTokSeq: seq[TokKind],
): Option[TokKind] =
  result = self.selParse(toHashSet(selTokSeq))


#proc selParse(
#  self: var Scone,
#  selTokSeq: seq[TokKind]
#): Option[TokKind] =
#  #result = none(TokKind)
#  for idx in 0 ..< selTokSeq.len():
#    self.stackSavedIlp()
#    let myTok = self.lexAndCheck(chk=false, 

proc loopSelParse(
  self: var Scone,
  #chk: bool,
  selProcSet: HashSet[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  #endTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
  haveForcedEndSepTok: bool=false,
): SppResult =
  #result = self.selParse(chk=true, selProcSet=selProcSet)[1]
  result.foundTok = none(TokKind)
  result.foundTok1 = none(TokKind)
  #echo "loopSelParse: result:" & $result

  var mySpp = self.selParse(chk=true, selProcSet=selProcSet)
  #echo "mySpp:" & $mySpp

  # result is if we found any valid token at all
  var didBreak: bool = false
  #var limitCnt: int = -1
  while mySpp[1].foundTok.isSome:
    result = mySpp[0](self=self, chk=false)
    if sepTok.isSome:
      self.stackSavedIlp()
      self.lex()
      if self.currTok.tok != sepTok.get():
        self.unstackSavedIlp()
        didBreak = true
        break
      self.unstackSavedIlp()
      self.lex()
    mySpp = self.selParse(chk=true, selProcSet=selProcSet)

  if not result.foundTok.isSome and not didBreak and haveOptEndSepTok:
      doAssert(
        not haveForcedEndSepTok
      )
      doAssert(
        sepTok.isSome
      )
      self.stackSavedIlp()
      self.lex()
      let haveSepTok = (
        self.currTok.tok == sepTok.get()
      )
      self.unstackSavedIlp()
      if haveSepTok:
        self.lex()
  if haveForcedEndSepTok:
    #echo "inside if haveForcedEndSepTok: " & $self.lexMain
    doAssert(
      not haveOptEndSepTok
    )
    doAssert(
      sepTok.isSome
    )
    if didBreak:
      self.lexAndExpect(sepTok.get())
    else:
      self.expect(sepTok.get())
      

proc loopSelParse(
  self: var Scone,
  selProcSeq: seq[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
  haveForcedEndSepTok: bool=false,
): SppResult =
  #var tempSelProcSeq: seq[SelParseProc]
  #for selProc in selProcArr:
  #  tempSelProcSeq.add sppSeq(selProc)
  result = self.loopSelParse(
    selProcSet=toHashSet(selProcSeq),
    sepTok=sepTok,
    haveOptEndSepTok=haveOptEndSepTok,
    haveForcedEndSepTok=haveForcedEndSepTok,
  )

# `req` is short for `required`
proc reqLoopSelParse(
  self: var Scone,
  #chk: bool,
  selProcSet: HashSet[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  #endTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
  haveForcedEndSepTok: bool=false,
): SppResult = 
  #echo "reqLoopSelParse(): " & $self.currTok
  result = self.loopSelParse(
    selProcSet=selProcSet,
    sepTok=sepTok,
    haveOptEndSepTok=haveOptEndSepTok,
    haveForcedEndSepTok=haveForcedEndSepTok,
  )
  if not result.foundTok.isSome:
    #echo "not result.foundTok.isSome: " & $result & " " & $self.currTok
    self.lexAndExpect(result.tokSet)

proc reqLoopSelParse(
  self: var Scone,
  selProcSeq: seq[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
): SppResult =
  result = self.reqLoopSelParse(
    selProcSet=toHashSet(selProcSeq),
    sepTok=sepTok,
    haveOptEndSepTok=haveOptEndSepTok,
  )

#proc optLoopSelParse(
#  self: var Scone,
#  selProcSet: HashSet[SelParseProc],
#  sepTok: Option[TokKind],
#  haveOptEndSepTok: bool
#) =
#  discard


#proc loopSelParse(
#  self: var Scone,
#  selTokSeq: seq[TokKind],
#  sepTok: Option[TokKind]=none(TokKind)
#) =
#  var selProcSet: seq[SelParseProc]


proc parseIdent(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseIdent(): begin: chk:" & $chk
  #let tempSet = toHashSet([tokIdent])
  discard doChkTok(tokIdent)
  let tempStr = self.currTok.optStr.get()
  #echo "parseIdent(): adding this ident: " & tempStr
  self.identStrS2d[^1].add tempStr

proc subParseIdentAssign(
  self: var Scone,
  chk: bool,
): SppResult =
  if chk:
    result.foundTok = none(TokKind)
    result.foundTok1 = none(TokKind)

    self.stackSavedIlp()
    self.lex()
    if self.currTok.tok == tokIdent:
      self.lex()
      if self.currTok.tok == tokAssign:
        result.foundTok = some(tokIdent)
        result.foundTok1 = some(tokAssign)
    self.unstackSavedIlp()
  else:
    #self.lexAndExpect(tokIdent)
    result = self.parseIdent(chk=false)
    self.lexAndExpect(tokAssign)

#proc parseIdentList(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  #discard doChkTok(tokIdent)
#  result = doChkSpp(parseIdent)
#
#  if self.lexAndCheck(chk=true, tokComma).isSome:
#    self.lex()
#    discard self.loopSelParse(
#      selProcSeq=(sppSeq @[parseIdent]),
#      sepTok=some(tokComma),
#      haveOptEndSepTok=false,
#    )
proc parseExpr(
  self: var Scone,
  chk: bool,
): SppResult
#proc parseType(
#  self: var Scone,
#  chk: bool,
#): SppResult
proc parseTypeWithOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult
proc parseTypeWithoutOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult
proc parseGenericFullImplList(
  self: var Scone,
  chk: bool,
): SppResult
proc parseGenericNamedImplList(
  self: var Scone,
  #chk: bool,
)

#proc parseTypeArrDim(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  discard doChkTok(tokLBracket)
#  #echo "parseTypeArrDim: lexMain: pre parseExpr: " & $self.lexMain
#  discard self.parseExpr(chk=false)
#  #echo "parseTypeArrDim: lexMain: post parseExpr: " & $self.lexMain
#  self.lexAndExpect(tokRBracket)

proc parseTypeBuiltinScalar(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTokSet(
    toHashSet([
      tokU8, tokU16, tokU32, tokU64,
      tokI8, tokI16, tokI32, tokI64,
      tokF32, tokF64,
      tokString, tokChar, tokVoid
    ])
  )

proc parseTypeToResolve(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseTypeToResolve() begin: chk:" & $chk
  result = doChkSpp(parseIdent)
  #discard self.parseIdent(chk=false)
  #echo "parseTypeToResolve() post ident `result`: " & $result
  #echo "parseTypeToResolve() post ident `lexMain`: " & $self.lexMain
  discard self.optParse(chk=false, selProc=spp parseGenericFullImplList)

proc parseTypeArray(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokArray)
  self.lexAndExpect(tokLBracket)

  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseExpr],
    sepTok=some(tokComma),
    haveOptEndSepTok=false,
  )
  self.lexAndExpect(tokSemicolon)
  discard self.parseTypeWithoutOptPreKwVar(chk=false)
  self.lexAndExpect(tokRBracket)

proc parseTypeMain(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = none(SppResultMain)
  #result.foundTok = none(TokKind)

  #let mySpp = self.selParse(
  #  chk=false,
  #  sppSeq @[
  #    parseTypeBuiltinScalar,
  #    parseTypeToResolve,
  #  ]
  #)
  #if chk:
  #  #echo "parseTypeMain: chk: " & $mySpp
  #  if mySpp[1].foundTok.isSome:
  #    #return some(mySpp.get()[1])
  #    return mySpp[1]
  #elif mySpp[1].foundTok.isSome:
  #  #echo "parseTypeMain: not chk: " & $mySpp
  #  #mySpp.get()(self=self, chk=chk)
  #  #let temp = mySpp.get()
  #  discard mySpp[0](self=self, chk=chk)
  #else:
  #  # this will be an error
  #  self.lexAndExpect(tokSet=mySpp[1].tokSet)
  #echo "debug: parseTypeMain(): start"
  discard doChkSelParse(
    sppSeq @[
      parseTypeBuiltinScalar,
      parseTypeToResolve,
      parseTypeArray,
    ],
    none(HashSet[TokKind])
  )

#proc subParseTypeWithOptPreKwVar(
#  self: var Scone,
#  chk: bool,
#): bool =
#  let myTok = self.lexAndCheck(
#    chk=true,
#    tokSet=toHashSet([tokVar, tokPtr]),
#  )
#  if myTok.isSome:
#    if myTok.get == tokPtr:
#      result = true

#proc haveVarPtrSubParseTypeWithOptPreKwVar(
#  self: var Scone,
#  chk: bool,
#): bool =

proc parseTypeWithOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult =
  result.foundTok = none(TokKind)

  var haveVar: bool = false
  var ptrDim: uint = 0
  var haveEither: bool = false

  let myVpTokSet = toHashSet([tokVar, tokPtr])
  result.tokSet = result.tokSet.union(myVpTokSet)

  var myTok = self.lexAndCheck(
    chk=true,
    tokSet=myVpTokSet,
  )

  if myTok.isSome:
    haveEither = true
    if myTok.get == tokVar:
      haveVar = true
      if chk:
        result.foundTok = some(myTok.get)
        return
      else: # if not chk:
        self.lex()
    else: # if myTok.get == tokPtr
      if chk:
        result.foundTok = some(myTok.get)
        return
      else: # if not chk:
        while myTok.isSome:
          myTok = self.selParse(
            selTokSet=toHashSet([tokPtr])
          )
          if myTok.isSome:
            self.lex()
          ptrDim += 1
  if chk and not haveEither:
    result = doChkSpp(parseTypeMain)
  
  discard self.parseTypeMain(chk=false)
  #discard self.optParse(chk=false, selProc=spp parseTypeArrDim)

proc parseTypeWithoutOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult =
  result.foundTok = none(TokKind)

  #var haveVar: bool = false
  var ptrDim: uint = 0
  var haveEither: bool = false

  let myVpTokSet = toHashSet([
    #tokVar, 
    tokPtr
  ])
  result.tokSet = result.tokSet.union(myVpTokSet)

  var myTok = self.lexAndCheck(
    chk=true,
    tokSet=myVpTokSet,
  )

  if myTok.isSome:
    haveEither = true
    if chk:
      result.foundTok = some(myTok.get)
      return
    else: # if not chk:
      while myTok.isSome:
        myTok = self.selParse(
          selTokSet=toHashSet([tokPtr])
        )
        if myTok.isSome:
          self.lex()
        ptrDim += 1
  if chk and not haveEither:
    result = doChkSpp(parseTypeMain)
  
  discard self.parseTypeMain(chk=false)
  #discard self.optParse(chk=false, selProc=spp parseTypeArrDim)


proc parseVarDeclEtcMost(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkSpp(parseIdent)
  self.lexAndExpect(tokColon)
  discard self.parseTypeWithoutOptPreKwVar(chk=false)

proc parseGenericDeclItem(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.parseIdent(chk=chk)
  if chk:
    return

proc parseGenericDeclList(
  self: var Scone,
  #chk: bool,
) =
  discard self.reqLoopSelParse(
    selProcSeq=(
      sppSeq @[parseGenericDeclItem]
    ),
    sepTok=some(tokComma),
    haveOptEndSepTok=true,
  )

proc subParseGenericDeclList(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLBracket)
  self.parseGenericDeclList(
    #chk=false
  )
  self.lexAndExpect(tokRBracket)

proc parseGenericNamedImplItem(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = self.parseIdent(chk=chk)
  #if chk:
  #  return
  #self.lexAndExpect(tokAssign)
  result = self.subParseIdentAssign(chk=chk)
  if not chk:
    discard self.parseTypeWithOptPreKwVar(chk=false)
  #discard self.parseType(chk=false)

proc parseGenericNamedImplList(
  self: var Scone,
  #chk: bool,
) =
  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseGenericNamedImplItem],
    sepTok=some(tokComma),
    haveOptEndSepTok=false,
  )

proc parseGenericUnnamedImplItem(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.parseTypeWithoutOptPreKwVar(chk=true)
  let haveNamed = self.subParseIdentAssign(chk=true)
  if chk:
    if not haveNamed.foundTok.isSome:
      return
    else:
      result.foundTok = none(TokKind)
      result.foundTok1 = none(TokKind)
  else: # if not chk:
    #if not haveNamed.foundTok.isSome:
    result = self.parseTypeWithoutOptPreKwVar(chk=false)
    #else:
    #  result
    #else:
    #  #result.tokSet
    #  #result.foundTok = none(TokKind)

proc parseGenericUnnamedImplList(
  self: var Scone,
  #chk: bool,
) =
  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseGenericUnnamedImplItem],
    sepTok=some(tokComma),
    haveOptEndSepTok=(
      #true
      false
    ),
  )

proc parseGenericFullImplList(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "debug start: chk:" & $chk & " " & $self.lexMain
  #discard doChkTokSet(
  #  toHashSet([tokGenericNamedArgListStart, tokLBracket])
  #)
  discard doChkTok(tokLBracket)
  #echo "debug: " & $result
  #echo "debug more: " & $self.lexMain

  #if result.foundTok.get() == tokGenericNamedArgListStart:
  #  self.parseGenericNamedImplList()
  #else: # if result.foundTok.get() == tokLBracket
  #  self.parseGenericUnnamedImplList()

  let haveNamed = self.subParseIdentAssign(chk=true)
  if not haveNamed.foundTok.isSome:
    self.parseGenericUnnamedImplList()
    if self.lexAndCheck(chk=true, tok=tokComma).isSome:
      self.lex()
      #discard self.optParse(
      #  chk=false,
      #  selProc=spp parseGenericNamedImplList,
      #)
      self.parseGenericNamedImplList()
  else:
    self.parseGenericNamedImplList()


  #self.lexAndCheck(chk=false, tok=tokRBrace)
  self.lexAndExpect(tokRBracket)



proc subParseFuncArgDeclList(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseIdent)
  self.lexAndExpect(tokColon)
  discard self.parseTypeWithOptPreKwVar(chk=false)
  #self.lexAndExpect(tokComma)

proc parseFuncArgDeclList(
  self: var Scone,
  chk: bool,
): SppResult =
  let haveNoArgs = self.lexAndCheck(
    chk=true,
    tok=(
      #tokResult
      tokRParen
    ),
  )
  if haveNoArgs.isSome:
    #discard doChkTok(tokResult)
    #discard self.lexAndCheck(chk=false, tok=tokColon)
    #discard self.parseTypeWithOptPreKwVar(chk=false)
    discard
  else: # if not haveNoArgs.isSome:
    #result = doChkSpp(subParseFuncArgDeclList)
    #echo "post result = doChkSpp(...)"

    discard self.loopSelParse(
      selProcSeq=(
        sppSeq @[subParseFuncArgDeclList]
      ),
      sepTok=some(tokComma),
      haveOptEndSepTok=true,
    )
    #discard self.lexAndCheck(chk=false, tok=tokResult)
    #discard self.lexAndCheck(chk=false, tok=tokColon)
    #discard self.parseTypeWithOptPreKwVar(chk=false)

#proc parseFuncArgUnnamedImplItem(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  result = self.parseTypeWithoutOptPreKwVar(chk=true)
#  let haveNamed = self.subParseIdentAssign(chk=true)
#  if chk:
#    if not haveNamed.foundTok.isSome:
#      return
#    else:
#      result.foundTok = none(TokKind)
#      result.foundTok1 = none(TokKind)
#  else: # if not chk:
#    #if not haveNamed.foundTok.isSome:
#    result = self.parseTypeWithoutOptPreKwVar(chk=false)
#    #else:
#    #  result
#    #else:
#    #  #result.tokSet
#    #  #result.foundTok = none(TokKind)

#proc parseFuncArgImplItem(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  result = doChkSpp(parseIdent)
#  self.lexAndExpect(tokAssign)
#  discard self.parseExpr(chk=false)
proc parseFuncNamedArgImplItem(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = self.parseIdent(chk=chk)
  #if chk:
  #  return
  #self.lexAndExpect(tokAssign)
  result = self.subParseIdentAssign(chk=chk)
  if not chk:
    discard self.parseTypeWithOptPreKwVar(chk=false)
  #discard self.parseType(chk=false)

proc parseFuncNamedArgImplList(
  self: var Scone,
  #chk: bool,
) =
  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseFuncNamedArgImplItem],
    sepTok=some(tokComma),
    haveOptEndSepTok=false,
  )

proc parseFuncUnnamedArgImplItem(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.parseTypeWithoutOptPreKwVar(chk=true)
  let haveNamed = self.subParseIdentAssign(chk=true)
  if chk:
    if not haveNamed.foundTok.isSome:
      return
    else:
      result.foundTok = none(TokKind)
      result.foundTok1 = none(TokKind)
  else: # if not chk:
    #if not haveNamed.foundTok.isSome:
    result = self.parseTypeWithoutOptPreKwVar(chk=false)
    #else:
    #  result
    #else:
    #  #result.tokSet
    #  #result.foundTok = none(TokKind)
proc parseFuncUnnamedArgImplList(
  self: var Scone,
  #chk: bool,
) =
  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseFuncUnnamedArgImplItem],
    sepTok=some(tokComma),
    haveOptEndSepTok=(
      #true
      false
    ),
  )

proc parseExprFuncCallPostGenericMain(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = self.parseFuncArgImplItem(chk=true)
  discard doChkTok(tokLParen)

  #if chk:
  #  return

  #result = self.loopSelParse(
  #  selProcSeq=(
  #    sppSeq @[parseFuncArgImplItem]
  #  ),
  #  sepTok=some(tokComma),
  #  haveOptEndSepTok=true,
  #)

  let haveNamed = self.subParseIdentAssign(chk=true)
  if not haveNamed.foundTok.isSome:
    self.parseFuncUnnamedArgImplList()
    if self.lexAndCheck(chk=true, tok=tokComma).isSome:
      self.lex()
      #discard self.optParse(
      #  chk=false,
      #  selProc=spp parseFuncNamedArgImplList,
      #)
      self.parseFuncNamedArgImplList()
  else:
    self.parseFuncNamedArgImplList()

  self.lexAndExpect(tokRParen)

proc parseExprFuncCallPostGeneric(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSelParse(
    sppSeq @[
      parseExprFuncCallPostGenericMain,
      parseExpr,
    ],
    none(HashSet[TokKind]),
  )[1]



proc parseFuncDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseFuncDecl(): begin: chk:" & $chk
  discard doChkTok(tokDef)
  #echo "parseFuncDecl(): post `tokDef`"
  discard self.parseIdent(chk=false)
  #echo "parseFuncDecl(): " & $self.currIdentStrSeq
  #discard self.subParseGenericDeclList(chk=false)
  discard self.optParse(chk=false, selProc=spp subParseGenericDeclList)

  self.lexAndExpect(tokLParen)
  # args go here
  discard self.parseFuncArgDeclList(chk=false)
  self.lexAndExpect(tokRParen)
  self.lexAndExpect(tokFuncReturnTypePrefix)
  discard self.parseTypeWithOptPreKwVar(chk=false)

  self.lexAndExpect(tokLBrace)
  # stmts go here
  self.lexAndExpect(tokRBrace)

  self.lexAndExpect(tokSemicolon)

proc parseStructDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokStruct)
  discard self.parseIdent(chk=false)
  discard self.optParse(chk=false, selProc=spp subParseGenericDeclList)

  self.lexAndExpect(tokLBrace)
  # fields go here
  #echo "test"
  discard self.loopSelParse(
    selProcSeq=(
      sppSeq @[parseVarDeclEtcMost]
    ),
    sepTok=some(tokSemicolon),
    haveForcedEndSepTok=true,
  )
  self.lexAndExpect(tokRBrace)
  self.lexAndExpect(tokSemicolon)

proc parseModule(
  self: var Scone,
  chk: bool,
) =
  self.lexAndExpect(tokModule)
  self.lexAndExpect(tokIdent)
  self.lexAndExpect(tokSemicolon)

  # AST stuff here
  #self.mkAst(
  #)

proc parseExprFuncCallPostIdent(
  self: var Scone,
  chk: bool,
): SppResult =

  #echo "parseExprFuncCallPostIdent(): pre: " & $self.lexMain
  #result = self.optParseThenExpectTokSeq(
  #  chk=chk,
  #  selProc=parseGenericFullImplList,
  #  postTokSeq=(@[tokFuncNamedArgListStart, tokLParen]),
  #)
  result = self.optParse(
    chk=chk,
    selProc=parseGenericFullImplList,
  )
  #echo "parseExprFuncCallPostIdent(): post: " & $self.lexMain
  #if chk:
  #  #echo "returning: " & $result
  #  if not result.foundTok.isSome:
  #    return
  #echo "testificate: " & $result
  #result.tokSet = result.tokSet.union(toHashSet([tokLParen]))
  let temp = self.parseExprFuncCallPostGeneric(chk=true)
  result.tokSet = result.tokSet.union(temp.tokSet)

  if chk:
    if result.foundTok.isSome:
      return
    else:
      result.foundTok = self.lexAndCheck(chk=true, tokSet=result.tokSet)
  else: # if not chk:
    if result.foundTok.isSome or temp.foundTok.isSome:
      result = self.parseExprFuncCallPostGeneric(chk=false)
    else:
      self.lexAndExpect(tokSet=result.tokSet)
    #discard self.optParseThenExpectTokSeq(
    #  chk=chk,
    #  selProc=parseExprFuncCallPostGeneric,
    #  postTokSeq=(@[tokRParen]),
    #)

proc parseExprIdentOrFuncCall(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseIdent)
  #echo "parseExprIdentOrFuncCall(): " & $chk & " " & $self.lexMain
  discard self.optParse(
    chk=false,
    selProc=spp parseExprFuncCallPostIdent,
  )

proc parseU64Lit(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokU64Lit)

proc subParseParenExpr(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLParen)
  discard self.parseExpr(chk=false)
  self.lexAndExpect(tokRParen)

proc parseExprLowestNonOp(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseExprLowestNonOp: " & "chk:" & $chk & " " & $self.lexMain
  result = doChkSelParse(
    sppSeq @[
      parseExprIdentOrFuncCall,
      parseU64Lit,
      subParseParenExpr,
    ],
    none(HashSet[TokKind]),
  )[1]

proc subOptParseExprBinop(
  self: var Scone,
  chk: bool,
  tokSet: HashSet[TokKind],
): SppResult = 
  proc tempParseFunc(
    self: var Scone,
    chk: bool,
  ): SppResult =
    discard doChkTokSet(tokSet)

  #echo "subOptParseExprBinop(): pre: " & $self.lexMain
  #result = self.optParse(
  #  chk=false,
  #  selProc=tempParseFunc, 
  #)
  #result = self.tempParseFunc(chk=true)
  #discard doChkTokSet(tokSet)
  result = self.tempParseFunc(chk=true)
  #echo (
  #  (
  #    "subOptParseExprBinop(): post: "
  #  ) & (
  #    $result & " " & $self.lexMain
  #  )
  #)
  if not chk:
    if result.foundTok.isSome:
      result = self.tempParseFunc(chk=false)
      #echo "testificate"
      discard self.parseExpr(chk=false)

proc subOptParseExprBinop(
  self: var Scone,
  chk: bool,
  tokSeq: seq[TokKind],
): SppResult =
  result = self.subOptParseExprBinop(
    chk=chk,
    tokSet=toHashSet(tokSeq),
  )
proc subOptParseExprBinop(
  self: var Scone,
  chk: bool,
  tok: TokKind,
): SppResult =
  result = self.subOptParseExprBinop(
    chk=chk,
    tokSet=toHashSet([tok]),
  )

proc parsePrefixUnary(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTokSet(toHashSet([
    tokPlus, 
    tokMinus, tokLogicNot, tokBitInvert,
    tokAddr
  ]))

proc parseExprSuffixFieldMethodAccess(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo (
  #  (
  #    "parseExprSuffixFieldMethodAccess(): pre tokDot: "
  #  ) & (
  #    $chk & " " & $self.lexMain
  #  )
  #)
  discard doChkTok(tokDot)
  #echo (
  #  (
  #    "parseExprSuffixFieldMethodAccess(): post tokDot: "
  #  ) & (
  #    $chk & " " & $self.lexMain
  #  )
  #)
  self.parseExprIdentOrFuncCall(chk=false)

proc parseExprSuffixDeref(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkTok(tokDeref)

proc parseExprSuffixArray(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLBrace)
  discard self.parseExpr(chk=false)
  discard doChkTok(tokRBrace)

proc parseExprFieldArrEtcChoice(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = self.selParse(
  #)
  let temp = self.selParse(
    chk=chk,
    selProcSeq=(
      sppSeq @[
        parseExprSuffixFieldMethodAccess,
        parseExprSuffixDeref,
        parseExprSuffixArray,
      ]
    )
  )
  if chk:
    result = temp[1]
  else: # if not chk:
    result.foundTok = temp[0](self=self, chk=false).foundTok

  #echo "parseExprFieldArrEtcChoice: post selParse: " & $chk & " " & $temp
  #result = temp[1]

proc parseExprFieldArrEtc(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprLowestNonOp)
  discard self.loopSelParse(
    selProcSeq=(
      sppSeq @[parseExprFieldArrEtcChoice]
    ),
  )

proc parseExprUnary(
  self: var Scone,
  chk: bool,
): SppResult = 
  #echo "parseExprUnary: lexMain: pre: " & $self.lexMain
  result = self.optParseThenExpectSpp(
    chk=chk,
    selProc=parsePrefixUnary,
    postSelProc=parseExprFieldArrEtc,
  )
  #echo "parseExprUnary: lexMain: post: " & $self.lexMain

proc parseExprMulDivMod(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprUnary)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokMul, tokDiv, tokMod]),
  )

proc parseExprAddSub(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprMulDivMod)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokPlus, tokMinus]),
  )

proc parseExprBitShift(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprAddSub)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokBitShl, tokBitShr]),
  )

proc parseExprCmpIneq(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprBitShift)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokCmpLt, tokCmpLe, tokCmpGt, tokCmpGe]),
  )

proc parseExprCmpEqNe(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprCmpIneq)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokCmpEq, tokCmpNe]),
  )

proc parseExprBitAnd(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprCmpEqNe)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokBitAnd,
  )

proc parseExprBitXor(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprBitAnd)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokBitXor,
  )

proc parseExprBitOr(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprBitXor)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokBitOr,
  )

proc parseExprLogicAnd(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprBitOr)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokLogicAnd,
  )

proc parseExprLogicOr(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = doChkSpp(parseExprLogicAnd)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokLogicOr,
  )

proc parseExpr(
  self: var Scone,
  chk: bool,
): SppResult =
  #discard doChkSelParse(
  #  sppSeq @[
  #    #parseExprLowestNonOp,
  #    parseExprLogicOr,
  #  ],
  #  none(HashSet[TokKind])
  #)
  result = self.parseExprLogicOr(chk=chk)

proc parseSrcFile*(
  self: var Scone
) =
  self.parseModule(chk=false)

  let mySppSeq = sppSeq @[
    parseFuncDecl,
    parseStructDecl,
  ]
  let temp = self.selParse(
    chk=true,
    mySppSeq
  )
  #echo "post self.selParse: " & $temp
  if temp[1].foundTok.isSome:
    #echo "test"
    discard self.loopSelParse(
      mySppSeq
    )
  else:
    # this *may* be an error
    self.lexAndExpect(
      temp[1].tokSet.union(toHashSet([tokEof]))
    )

  self.lexAndExpect(tokEof)
  #let temp = self.loopSelParse()
  #if not temp.foundTok.isSome:
  #  discard


