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
  result.foundTok = none(TokKind)
  let hiddenMyTok = self.lexAndCheck(chk=chk, argTokSet)
  if chk:
    if hiddenMyTok.isSome:
      #return some(SppResultMain(
      #  tokSet: argTokSet,
      #  foundTok: hiddenMyTok.get(),
      #))
      #return SppResult(
      #  tokSet: argTokSet,
      #  foundTok: hiddenMyTok,
      #)
      result.foundTok = hiddenMyTok
    #else:
    #  #return none(SppResultMain)
    #  return
    return
  hiddenMyTok

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
  hiddenMySppRet

template doChkSelParse(
  someSppSeq: untyped,
  optLastTokSet: untyped,
): untyped = 
  result.foundTok = none(TokKind)
  let hiddenMySpp = self.selParse(
    chk=false,
    #sppSeq @[
    #  parseIdent,
    #  subParseParenExpr,
    #],
    someSppSeq
  )
  if chk:
    if hiddenMySpp[1].foundTok.isSome:
      return hiddenMySpp[1]
  elif hiddenMySpp[1].foundTok.isSome:
    discard hiddenMySpp[0](self=self, chk=chk)
  else:
    # this will be an error
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
      result = selProc(self=self, chk=false)
  #let sppRet = selProc(self=self, chk=true)
  #if sppRet.foundTok.isSome:
  #  if chk:
  #    result = sppRet
  #  else: # if not chk
  #    self.unstackSavedIlp()
  #    return selProc(self=self, chk=false)
  #self.unstackSavedIlp()

proc optParseThenExpect(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
  postTokSet: HashSet[TokKind],
): SppResult =
  result = self.optParse(
    chk=true,
    selProc=selProc,
  )
  result.tokSet = result.tokSet.union(postTokSet)
  self.stackSavedIlp()
  self.lexAndExpect(result.tokSet)
  self.unstackSavedIlp()
  discard self.optParse(
    chk=chk,
    selProc=selProc,
  )
  #if not result.foundTok.isSome:
  #  result.tokSet = result.tokSet.union(postTokSet)
  #  self.lexAndExpect(result.tokSet)
  #else:
  #  self.lexAndExpect(postTokSet)

proc optParseThenExpect(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
  postTokSeq: seq[TokKind],
): SppResult =
  result = self.optParseThenExpect(
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
  let temp = postSelProc(
    self=self,
    chk=true
  )
  #result.tokSet = result.tokSet.union(temp.tokSet)
  result = self.optParseThenExpect(
    chk=chk,
    selProc=selProc,
    postTokSet=temp.tokSet,
  )
  


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
): SppResult =
  result = self.selParse(chk=true, selProcSet=selProcSet)[1]
  #echo "loopSelParse: result:" & $result

  var mySpp = self.selParse(chk=false, selProcSet=selProcSet)
  #echo "mySpp:" & $mySpp

  # result is if we found any valid token at all
  var didBreak: bool = false
  #var limitCnt: int = -1
  while mySpp[1].foundTok.isSome:
    discard mySpp[0](self=self, chk=false)
    if sepTok.isSome:
      self.stackSavedIlp()
      self.lex()
      if self.currTok.tok != sepTok.get():
        self.unstackSavedIlp()
        didBreak = true
        break
      self.unstackSavedIlp()
      self.lex()
    mySpp = self.selParse(chk=false, selProcSet=selProcSet)

  if not result.foundTok.isSome and not didBreak and haveOptEndSepTok:
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

proc loopSelParse(
  self: var Scone,
  selProcSeq: seq[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
): SppResult =
  #var tempSelProcSeq: seq[SelParseProc]
  #for selProc in selProcArr:
  #  tempSelProcSeq.add sppSeq(selProc)
  result = self.loopSelParse(
    selProcSet=toHashSet(selProcSeq),
    sepTok=sepTok,
    haveOptEndSepTok=haveOptEndSepTok,
  )

# `req` is short for `required`
proc reqLoopSelParse(
  self: var Scone,
  #chk: bool,
  selProcSet: HashSet[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  #endTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
): SppResult = 
  #echo "reqLoopSelParse(): " & $self.currTok
  result = self.loopSelParse(
    selProcSet=selProcSet,
    sepTok=sepTok,
    haveOptEndSepTok=haveOptEndSepTok,
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

proc parseIdentList(
  self: var Scone,
  chk: bool,
): SppResult =
  #discard doChkTok(tokIdent)
  discard doChkSpp(parseIdent)

  if self.lexAndCheck(chk=true, tokComma).isSome:
    self.lex()
    discard self.loopSelParse(
      selProcSeq=(sppSeq @[parseIdent]),
      sepTok=some(tokComma),
      haveOptEndSepTok=false,
    )
proc parseExpr(
  self: var Scone,
  chk: bool,
): SppResult
proc parseTypeWithOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult
proc subParseGenericImplList(
  self: var Scone,
  chk: bool,
): SppResult
proc parseGenericImplList(
  self: var Scone,
  #chk: bool,
)

proc parseTypeArrDim(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLBracket)
  discard self.parseExpr(chk=false)
  self.lexAndExpect(tokRBracket)

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
  discard doChkSpp(parseIdent)
  discard self.optParse(chk=false, selProc=spp subParseGenericImplList)

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
  discard doChkSelParse(
    sppSeq @[
      parseTypeBuiltinScalar,
      parseTypeToResolve,
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
  #proc doParseVarPtr(
  #  self: var Scone,
  #  chk: bool,
  #): bool =
  #  result = false
  #  #var myTok = doChkTok([tokVar, tokPtr])
  #  #if myTok.isSome:
  #  #  if myTok.get == tokPtr:
  #  #    myTok = self.lexAndCheck(chk=true, tok=tokPtr)
  #  #    result = true

  #result = none(SppResultMain)
  result.foundTok = none(TokKind)

  var haveVar: bool = false
  var ptrDim: uint = 0
  var haveEither: bool = false
  #self.stackSavedIlp()
  #self.lex()
  #echo "test 2: " & $self.currTok
  #self.unstackSavedIlp()
  let myVpTokSet = toHashSet([tokVar, tokPtr])
  result.tokSet = result.tokSet.union(myVpTokSet)

  var myTok = self.lexAndCheck(
    chk=true,
    tokSet=myVpTokSet,
  )

  if myTok.isSome:
    #echo "myTok.isSome: before: " & $myTok
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
        #echo "test: tokPtr: begin"
        while myTok.isSome:
          #echo $myTok
          myTok = self.selParse(
            selTokSet=toHashSet([tokPtr])
          )
          if myTok.isSome:
            self.lex()
          ptrDim += 1
  if chk and not haveEither:
    #echo "chk and not haveEither: begin"
    #self.stackSavedIlp()
    #self.lex()
    #echo "test 0: " & $self.currTok
    #self.unstackSavedIlp()
    discard doChkSpp(parseTypeMain)
  
  #echo "haveVar:" & $haveVar & " ptrDim: " & $ptrDim

  #self.stackSavedIlp()
  #self.lex()
  #echo "test 1: " & $self.currTok
  #self.unstackSavedIlp()
  discard self.parseTypeMain(chk=false)
  discard self.optParse(chk=false, selProc=spp parseTypeArrDim)

  #echo "test 4: " & $self.currTok

  #self.stackSavedIlp()
  #self.lex()
  #echo "test 3: " & $self.currTok
  #self.unstackSavedIlp()


#proc parseVarDeclEtcMost(
#  self: var Scone,
#  chk: bool,
#): bool =

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
  discard doChkTok(tokLBrace)
  self.parseGenericDeclList(
    #chk=false
  )
  self.lexAndExpect(tokRBrace)

proc parseGenericImplItem(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.parseIdent(chk=chk)
  if chk:
    return
  self.lexAndExpect(tokAssign)
  discard self.parseTypeWithOptPreKwVar(chk=false)

proc parseGenericImplList(
  self: var Scone,
  #chk: bool,
) =
  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseGenericImplItem],
    sepTok=some(tokComma),
    haveOptEndSepTok=true,
  )

proc subParseGenericImplList(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLBrace)
  self.parseGenericImplList()
  #self.lexAndCheck(chk=false, tok=tokRBrace)
  self.lexAndExpect(tokRBrace)



proc subParseFuncArgDeclList(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkSpp(parseIdentList)
  self.lexAndExpect(tokColon)
  discard self.parseTypeWithOptPreKwVar(chk=false)
  self.lexAndExpect(tokComma)

proc parseFuncArgDeclList(
  self: var Scone,
  chk: bool,
): SppResult =
  let haveNoArgs = self.lexAndCheck(
    chk=true,
    tok=tokResult,
  )
  if haveNoArgs.isSome:
    discard doChkTok(tokResult)
    discard self.lexAndCheck(chk=false, tok=tokColon)
    discard self.parseTypeWithOptPreKwVar(chk=false)
  else:
    discard doChkSpp(subParseFuncArgDeclList)

    discard self.loopSelParse(
      selProcSeq=(
        sppSeq @[subParseFuncArgDeclList]
      ),
      sepTok=some(tokComma),
      haveOptEndSepTok=true,
    )
    discard self.lexAndCheck(chk=false, tok=tokResult)
    discard self.lexAndCheck(chk=false, tok=tokColon)
    discard self.parseTypeWithOptPreKwVar(chk=false)

proc parseFuncArgImplItem(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkSpp(parseIdent)
  self.lexAndExpect(tokAssign)
  discard self.parseExpr(chk=false)

proc parseFuncArgImplList(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.parseFuncArgImplItem(chk=true)

  if chk:
    return

  result = self.loopSelParse(
    selProcSeq=(
      sppSeq @[parseFuncArgImplItem]
    ),
    sepTok=some(tokComma),
    haveOptEndSepTok=true,
  )



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

  self.lexAndExpect(tokLParen)
  # stmts go here
  self.lexAndExpect(tokRParen)

  self.lexAndExpect(tokSemicolon)

proc parseStructDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokStruct)
  discard self.parseIdent(chk=false)
  discard self.optParse(chk=false, selProc=spp subParseGenericDeclList)

  self.lexAndExpect(tokLParen)
  # fields go here
  self.lexAndExpect(tokRParen)
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
  doAssert(
    not chk,
    "eek! " & $self.currTok
  )
  #result = self.optParse(
  #  chk=true,
  #  selProc=subParseGenericImplList,
  #)
  #result.tokSet = haveGenerics.tokSet
  #if result.foundTok.isSome:
  #  discard
  #result = self.selParse(
  #  chk=true,
  #  selProcSeq=sppSeq @[subParseGenericImplList],
  #)[1]
  #var haveGeneric: bool = false
  #if result.foundTok.isSome:

  #result = self.optParse(
  #  chk=false,
  #  selProc=subParseGenericImplList,
  #)
  #if not result.foundTok.isSome:
  #  result.tokSet = result.tokSet.union(toHashSet([tokLParen]))
  #  self.lexAndExpect(result.tokSet)
  #else:
  #  self.lexAndExpect(tokLParen)
  #discard self.optParse(
  #  chk=false,
  #  selProc=parseFuncArgImplList
  #)
  result = self.optParseThenExpect(
    chk=false,
    selProc=subParseGenericImplList,
    postTokSeq=(@[tokLParen]),
  )
  discard self.optParseThenExpect(
    chk=false,
    selProc=parseFuncArgImplList,
    postTokSeq=(@[tokRParen]),
  )

proc parseExprIdentOrFuncCall(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkSpp(parseIdent)
  discard self.optParse(
    chk=false,
    selProc=spp parseExprFuncCallPostIdent,
  )

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
  discard doChkSelParse(
    sppSeq @[
      parseExprIdentOrFuncCall,
      subParseParenExpr,
    ],
    none(HashSet[TokKind]),
  )

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

  result = self.optParse(
    chk=false,
    selProc=tempParseFunc, 
  )
  if result.foundTok.isSome:
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
    tokPlus, tokMinus, tokLogicNot, tokBitInvert,
    tokAddr
  ]))

proc parseExprFieldArrEtcChoice(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = self.selParse(
  #)
  discard

proc parseExprFieldArrEtc(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkSpp(parseExprLowestNonOp)
  self.loopSelParse(
    selProcSeq=(
      sppSeq @[parseExprFieldArrEtcChoice]
    ),
  )

proc parseExprUnary(
  self: var Scone,
  chk: bool,
): SppResult = 
  result = self.optParseThenExpectSpp(
    chk=chk,
    selProc=parsePrefixUnary,
    postSelProc=parseExprFieldArrEtc,
  )

proc parseExprMulDivMod(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprUnary)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokMul, tokDiv, tokMod]),
  )

proc parseExprAddSub(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprMulDivMod)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokPlus, tokMinus]),
  )

proc parseExprBitShift(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprAddSub)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokBitShl, tokBitShr]),
  )

proc parseExprCmpIneq(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprBitShift)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokCmpLt, tokCmpLe, tokCmpGt, tokCmpGe]),
  )

proc parseExprCmpEqNe(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprCmpIneq)
  discard self.subOptParseExprBinop(
    chk=false,
    tokSeq=(@[tokCmpEq, tokCmpNe]),
  )

proc parseExprBitAnd(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprCmpEqNe)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokBitAnd,
  )

proc parseExprBitXor(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprBitAnd)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokBitXor,
  )

proc parseExprBitOr(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprBitXor)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokBitOr,
  )

proc parseExprLogicAnd(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprBitOr)
  discard self.subOptParseExprBinop(
    chk=false,
    tok=tokLogicAnd,
  )

proc parseExprLogicOr(
  self: var Scone,
  chk: bool,
): SppResult = 
  discard doChkSpp(parseExprLogicAnd)
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
    self.lexAndExpect(
      temp[1].tokSet.union(toHashSet([tokEof]))
    )

  self.lexAndExpect(tokEof)
  #let temp = self.loopSelParse()
  #if not temp.foundTok.isSome:
  #  discard


