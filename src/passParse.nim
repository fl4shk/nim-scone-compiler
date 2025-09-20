import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import scone
import lex

#template parent(
#  child: AstNode
#): untyped =
#  self.ast[child.parentIdx]
#template currAst(): untyped =
#  self.ast[self.currAstIdx]
#template currParent(): untyped =
#  currAst.parent

#template myAst(): untyped =
#  self.ast

#template stack(
#  arg: untyped
#): untyped =
#  myAst = arg
#  myAst
#
#template unstack(): untyped =
#  myAst = myAst.parent
#  myAst
  
proc mkAstMain*(
  self: var Scone,
  kind: AstKind,
  #optParent: Option[AstNode]=none(AstNode),
  #ast: AstT,
): AstNode = 
  result = AstNode(
    lexMain: self.lexMain,
    #parent: optParent.get(),
    kind: kind,
    #ast,
  )
  #if optParent.isSome:
  #  result = AstNode(
  #    lexMain: self.lexMain,
  #    #parent: optParent.get(),
  #    kind: kind,
  #    #ast,
  #  )
  #  #result.parent = optParent.get()
  #else:
  #  #result.parent = self.ast
  #  result = AstNode(
  #    lexMain: self.lexMain,
  #    #parent: myAst,
  #    kind: kind,
  #    #ast,
  #  )
  ##echo myAst.repr()
  ##echo "mkAst(): begin:"
  ##echo $result.ast
  ##echo "mkAst(): end:"

#proc mkAstAndStackMain*(
#  self: var Scone,
#  kind: AstKind,
#  optParent: Option[AstNode]=none(AstNode),
#): AstNode =
#  result = self.mkAst(
#    kind=kind,
#    optParent=optParent,
#  ).stack()

template mkAst*(
  kind: AstKind,
  #optParent: Option[AstNode]=none(AstNode),
): untyped =
  self.mkAstMain(
    kind,
    #optParent,
  )
  #self.mkAstAndStackMain(kind, optParent)
  #defer: discard unstack()


#template mkAstAndStack(
#  kind: AstKind,
#  optParent: Option[AstNode]=none(AstNode),
#): untyped =
#  self.mkAstAndStackMain(kind, optParent)

#proc mkAst*(
#  self: var Scone,
#  tok: TokKind,
#  litVal: Option[AstLitVal],
#  symIdxSeq: seq[uint64],
#  parentIdx: uint64,
#) =
#  let toAdd = AstNode(
#    tok: tok,
#    lineNum: self.lineNum,
#    litVal: litVal,
#    symIdxSeq: symIdxSeq,
#    parentIdx: parentIdx,
#  )
#  toAdd.parent.chIdxSeq.add uint64(self.ast.len())
#  self.ast.add toAdd

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
    foundSelProc*: Option[SelParseProc]
    ast*: AstNode
  #SppResult* = SppResult

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

template doChkTokSetWithChk(
  argChk: untyped,
  argTokSet: untyped,
): untyped = 
  #result = none(SppResultMain)
  #result = SppResult(foundTok: none(TokKind))
  result.tokSet = argTokSet
  #result.foundTok = none(TokKind)
  #let hiddenMyTok = 
  result.foundTok = (
    self.lexAndCheck(chk=argChk, argTokSet)
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

template doChkTokSet(
  argTokSet: untyped,
): untyped =
  doChkTokSetWithChk(chk, argTokSet)

template doChkTok(
  argTok: untyped,
): untyped =
  let hiddenArgTokSet = toHashSet([argTok])
  doChkTokSet(
    hiddenArgTokSet
  )

template doChkSpp(
  selProc: untyped,
  optTokSet: Option[HashSet[TokKind]]=none(HashSet[TokKind]),
): untyped =
  #result = none(SppResultMain)
  #result = SppResult(foundTok: none(TokKind))
  result.foundTok = none(TokKind)
  var hiddenMySppRet = selProc(self=self, chk=chk)
  if optTokSet.isSome:
    hiddenMySppRet.tokSet = hiddenMySppRet.tokSet.union(optTokSet.get())
  if chk:
    return hiddenMySppRet
  #result = hiddenMySppRet
  #result
  hiddenMySppRet

template doChkSelParse(
  someSppSeq: untyped,
  optLastTokSet: untyped,
): untyped = 
  result.foundTok = none(TokKind)
  #echo "doChkSelParse(): begin: chk:" & $chk
  var hiddenMySpp = self.selParse(
    chk=false,
    #sppSeq @[
    #  parseIdent,
    #  subParseParenExpr,
    #],
    someSppSeq
  )
  #echo "doChkSelParse(): post selParse: " & $self.lexMain
  if chk:
    if hiddenMySpp.foundTok.isSome:
      #echo "doChkSelParse(): chk==true, returning: " & $hiddenMySpp
      return hiddenMySpp
  elif hiddenMySpp.foundTok.isSome:
    hiddenMySpp = hiddenMySpp.foundSelProc.get()(self=self, chk=chk)
  else:
    # this will be an error
    #echo "this will be an error"
    if optLastTokSet.isSome:
      self.lexAndExpect(
        tokSet=hiddenMySpp.tokSet.union(optLastTokSet.get())
      )
    else:
      self.lexAndExpect(
        tokSet=hiddenMySpp.tokSet
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

#proc optParseThenExpectTokSet(
#  self: var Scone,
#  chk: bool,
#  selProc: SelParseProc,
#  postTokSet: HashSet[TokKind],
#): SppResult =
#  result = selProc(self=self, chk=true)
#  if chk:
#    #let postResult = postSelProc(self=self, chk=true)
#    let postFoundTok = self.lexAndCheck(chk=true, tokSet=postTokSet)
#    if result.foundTok.isSome:
#      discard
#    elif postFoundTok.isSome:
#      result.foundTok = postFoundTok
#    result.tokSet = result.tokSet.union(postTokSet)
#  else: # if not chk:
#    if not result.foundTok.isSome:
#      let postFoundTok = self.lexAndCheck(chk=true, tokSet=postTokSet)
#      if not postFoundTok.isSome:
#        # this will be an error
#        self.lexAndExpect(
#          tokSet=result.tokSet.union(postTokSet)
#        )
#      else: # if postFoundTok.isSome:
#        self.lexAndExpect(tokSet=postTokSet)
#    else: # if result.foundTok.isSome:
#      result = selProc(self=self, chk=false)
#      self.lexAndExpect(tokSet=postTokSet)
#
#proc optParseThenExpectTokSeq(
#  self: var Scone,
#  chk: bool,
#  selProc: SelParseProc,
#  postTokSeq: seq[TokKind],
#): SppResult =
#  result = self.optParseThenExpectTokSet(
#    chk=chk,
#    selProc=selProc,
#    postTokSet=toHashSet(postTokSeq),
#  )
#
#proc optParseThenExpectSpp(
#  self: var Scone,
#  chk: bool,
#  selProc: SelParseProc,
#  postSelProc: SelParseProc,
#): SppResult =
#  result = selProc(self=self, chk=true)
#  if chk:
#    let postResult = postSelProc(self=self, chk=true)
#    if result.foundTok.isSome:
#      discard
#    elif postResult.foundTok.isSome:
#      result.foundTok = postResult.foundTok
#    result.tokSet = result.tokSet.union(postResult.tokSet)
#  else: # if not chk
#    if not result.foundTok.isSome:
#      let postResult = postSelProc(self=self, chk=true)
#      if not postResult.foundTok.isSome:
#        # this will be an error
#        self.lexAndExpect(
#          tokSet=result.tokSet.union(postResult.tokSet),
#        )
#      else: # if postFoundTok.isSome:
#        result = postSelProc(self=self, chk=false)
#    else: # if result.foundTok.isSome:
#      result = selProc(self=self, chk=false)
#      discard postSelProc(self=self, chk=false)
  


proc selParse(
  self: var Scone,
  chk: bool,
  selProcSet: OrderedSet[SelParseProc],
): SppResult =
  #for myProc in selProcSet:
  #  if myProc[](self, true):
  #    return myProc
  #for idx in 0 ..< selProcSet.len():
  result.foundTok = none(TokKind)
  for selProc in selProcSet:
    self.stackSavedIlp()
    let sppRet = selProc(self=self, chk=true)
    result.tokSet = result.tokSet.union(sppRet.tokSet)

    if sppRet.foundTok.isSome:
      if chk:
        result.foundSelProc = some(selProc)
        result.foundTok = sppRet.foundTok
      else:
        self.unstackSavedIlp()
        result = sppRet
        result.foundSelProc = some(selProc)
        return
        #return (selProc, sppRet)
        
    self.unstackSavedIlp()
    
  #echo "selParse(): returning `none`"

proc selParse(
  self: var Scone,
  chk: bool,
  selProcSeq: seq[SelParseProc],
): SppResult =
  result = self.selParse(chk=chk, selProcSet=toOrderedSet(selProcSeq))
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
  selProcSet: OrderedSet[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  #endTok: Option[TokKind]=none(TokKind),
  haveOptEndSepTok: bool=false,
  haveForcedEndSepTok: bool=false,
): SppResult =
  #result = self.selParse(chk=true, selProcSet=selProcSet)
  result.foundTok = none(TokKind)
  result.foundTok1 = none(TokKind)
  #echo "loopSelParse: result:" & $result

  var mySpp = self.selParse(chk=true, selProcSet=selProcSet)
  let foundFirst = mySpp.foundTok.isSome
  #echo "mySpp:" & $mySpp

  # result is if we found any valid token at all
  var didBreak: bool = false
  #var limitCnt: int = -1
  var mySpp1: SppResult
  var atFirstIter: bool = true
  while mySpp.foundTok.isSome:
    mySpp1 = mySpp.foundSelProc.get()(self=self, chk=false)
    if atFirstIter:
      result = mySpp1
      atFirstIter = false
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

  if haveOptEndSepTok:
    doAssert(
      not haveForcedEndSepTok
    )

  if not mySpp1.foundTok.isSome and not didBreak and haveOptEndSepTok:
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
  if foundFirst and haveForcedEndSepTok:
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
    selProcSet=toOrderedSet(selProcSeq),
    sepTok=sepTok,
    haveOptEndSepTok=haveOptEndSepTok,
    haveForcedEndSepTok=haveForcedEndSepTok,
  )

# `req` is short for `required`
proc reqLoopSelParse(
  self: var Scone,
  #chk: bool,
  selProcSet: OrderedSet[SelParseProc],
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
    selProcSet=toOrderedSet(selProcSeq),
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
  #self.identStrS2d[^1].add tempStr
  result.ast = mkAst(astIdent)
  result.ast.myIdent.strVal = tempStr

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
proc parseExprFieldArrEtc(
  self: var Scone,
  chk: bool,
): SppResult
proc parseExprLowestNonOp(
  self: var Scone,
  chk: bool,
): SppResult
proc parseExprUnary(
  self: var Scone,
  chk: bool,
): SppResult
#proc parseExprAddrLhsMain(
#  self: var Scone,
#  chk: bool,
#): SppResult
#proc parseExprLhsMain(
#  self: var Scone,
#  chk: bool,
#): SppResult
proc parseExprLhs(
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
proc parseStmt(
  self: var Scone,
  chk: bool,
): SppResult

#proc parseTypeArrDim(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  discard doChkTok(tokLBracket)
#  #echo "parseTypeArrDim: lexMain: pre parseExpr: " & $self.lexMain
#  discard self.parseExpr(chk=false)
#  #echo "parseTypeArrDim: lexMain: post parseExpr: " & $self.lexMain
#  self.lexAndExpect(tokRBracket)

proc parseTypeBasicBuiltin(
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
  result.ast = mkAst(astBasicType)
  case result.foundTok.get():
  of tokU8:
    result.ast.myBasicType.kind = basicTypeU8
  of tokU16:
    result.ast.myBasicType.kind = basicTypeU16
  of tokU32:
    result.ast.myBasicType.kind = basicTypeU32
  of tokU64:
    result.ast.myBasicType.kind = basicTypeU64
  of tokI8:
    result.ast.myBasicType.kind = basicTypeI8
  of tokI16:
    result.ast.myBasicType.kind = basicTypeI16
  of tokI32:
    result.ast.myBasicType.kind = basicTypeI32
  of tokI64:
    result.ast.myBasicType.kind = basicTypeI64
  of tokF32:
    result.ast.myBasicType.kind = basicTypeF32
  of tokF64:
    result.ast.myBasicType.kind = basicTypeF64
  of tokString:
    result.ast.myBasicType.kind = basicTypeString
  of tokChar:
    result.ast.myBasicType.kind = basicTypeChar
  of tokVoid:
    result.ast.myBasicType.kind = basicTypeVoid
  else:
    doAssert(
      false,
      "eek! " & $result
    )

proc parseTypeToResolve(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseTypeToResolve() begin: chk:" & $chk
  result = doChkSpp(parseIdent)
  var myIdent = result.ast
  result.ast = mkAst(astNamedType)
  result.ast.myNamedType.ident = myIdent
  #discard self.parseIdent(chk=false)
  #echo "parseTypeToResolve() post ident `result`: " & $result
  #echo "parseTypeToResolve() post ident `lexMain`: " & $self.lexMain
  var temp = (
    self.optParse(chk=false, selProc=spp parseGenericFullImplList)
  )
  if temp.foundTok.isSome:
    result.ast.myNamedType.genericImpl = temp.ast
  else:
    result.ast.myNamedType.genericImpl = mkAst(astGenericList)

proc parseTypeArray(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokArray)
  result.ast = mkAst(kind=astArray)
  #defer: discard unstack

  self.lexAndExpect(tokLBracket)

  #discard self.loopSelParse(
  #  selProcSeq=sppSeq @[parseExpr],
  #  sepTok=some(tokComma),
  #  haveOptEndSepTok=false,
  #)
  result.ast.myArray.dim = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokSemicolon)
  result.ast.myArray.elemType = (
    self.parseTypeWithoutOptPreKwVar(chk=false).ast
  )
  self.lexAndExpect(tokRBracket)
  #discard unstack()
proc parseTypeOpenarray(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokOpenarray)
  result.ast = mkAst(kind=astOpenarray)
  self.lexAndExpect(tokLBracket)
  result.ast.myOpenarray.elemType = (
    self.parseTypeWithoutOptPreKwVar(chk=false).ast
  )
  self.lexAndExpect(tokRBracket)

proc parseTypeMainBuiltin(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSelParse(
    sppSeq @[
      parseTypeBasicBuiltin,
      parseTypeArray,
      parseTypeOpenarray,
    ],
    none(HashSet[TokKind])
  )

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
  #  if mySpp.foundTok.isSome:
  #    #return some(mySpp.get())
  #    return mySpp
  #elif mySpp.foundTok.isSome:
  #  #echo "parseTypeMain: not chk: " & $mySpp
  #  #mySpp.get()(self=self, chk=chk)
  #  #let temp = mySpp.get()
  #  discard mySpp(self=self, chk=chk)
  #else:
  #  # this will be an error
  #  self.lexAndExpect(tokSet=mySpp.tokSet)
  #echo "debug: parseTypeMain(): start"
  result = doChkSelParse(
    sppSeq @[
      parseTypeMainBuiltin,
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
proc parseTypeBuiltinWithoutOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult =
  result.foundTok = none(TokKind)

  var ptrDim: uint = 0
  var haveEither: bool = false

  let myVpTokSet = toHashSet([
    #tokVar, 
    tokPtr
  ])
  #result.tokSet = result.tokSet.union(myVpTokSet)
  let tempTokSet = result.tokSet.union(
    self.parseTypeMain(chk=true).tokSet
  )

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
      result.tokSet = tempTokSet
  if chk and not haveEither:
    result = doChkSpp(parseTypeMain, some(tempTokSet))
  
  result.ast = mkAst(kind=astType)
  if haveEither:
    if ptrDim == 0:
      discard
    else:
      result.ast.myType.ptrDim = ptrDim
  result.ast.myType.child = self.parseTypeMainBuiltin(chk=false).ast

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
  let tempTokSet = result.tokSet.union(
    self.parseTypeMain(chk=true).tokSet
  )

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
        result.tokSet = result.tokSet.union(tempTokSet)
        return
      else: # if not chk:
        self.lex()
    else: # if myTok.get == tokPtr
      if chk:
        result.foundTok = some(myTok.get)
        result.tokSet = result.tokSet.union(tempTokSet)
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
    result = doChkSpp(parseTypeMain, some(tempTokSet))
  
  #discard self.parseTypeMain(chk=false)
  #discard self.optParse(chk=false, selProc=spp parseTypeArrDim)
  result.ast = mkAst(kind=astType)
  #defer: discard unstack()
  if haveEither:
    if ptrDim == 0:
      result.ast.myType.kwVar = true
    else:
      result.ast.myType.ptrDim = ptrDim
  result.ast.myType.child = self.parseTypeMain(chk=false).ast

proc parseTypeWithoutOptPreKwVar(
  self: var Scone,
  chk: bool,
): SppResult =
  result.foundTok = none(TokKind)
  #defer: discard unstack()

  #var haveVar: bool = false
  var ptrDim: uint = 0
  var haveEither: bool = false

  let myVpTokSet = toHashSet([
    #tokVar, 
    tokPtr
  ])
  #result.tokSet = result.tokSet.union(myVpTokSet)
  let tempTokSet = result.tokSet.union(
    self.parseTypeMain(chk=true).tokSet
  )

  var myTok = self.lexAndCheck(
    chk=true,
    tokSet=myVpTokSet,
  )

  if myTok.isSome:
    haveEither = true
    if chk:
      result.foundTok = some(myTok.get)
      #result.tokSet = result.tokSet.union(
      #)
      #result.tokSet = tempTokSet
      return
    else: # if not chk:
      while myTok.isSome:
        myTok = self.selParse(
          selTokSet=toHashSet([tokPtr])
        )
        if myTok.isSome:
          self.lex()
          ptrDim += 1
      result.tokSet = tempTokSet
  if chk and not haveEither:
    result = doChkSpp(parseTypeMain, some(tempTokSet))
  
  result.ast = mkAst(kind=astType)
  #defer: discard unstack()
  if haveEither:
    if ptrDim == 0:
      #result.ast.myType.varPtrSeq.add self.mkAst(astVar)
      #result.ast.myType.kwVar = true
      discard
    else:
      result.ast.myType.ptrDim = ptrDim
  result.ast.myType.child = self.parseTypeMain(chk=false).ast
  #discard self.optParse(chk=false, selProc=spp parseTypeArrDim)


proc parseVarEtcDeclMost(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseIdent)
  var myIdent = result.ast
  result.ast = mkAst(astVarEtcDeclMost)
  result.ast.myVarEtcDeclMost.ident = myIdent

  self.lexAndExpect(tokColon)
  result.ast.myVarEtcDeclMost.type = (
    self.parseTypeWithoutOptPreKwVar(chk=false).ast
  )

proc parseGenericDeclItem(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.parseIdent(chk=chk)
  if chk:
    return
  else: # if not chk:
    if result.foundTok.isSome:
      template parent(): untyped =
        self.parentTempSeq[^1]
      case parent.kind:
      of astDef:
        #if parent.myDef.genericDecl == nil:
        if not parent.myDef.genericDecl.isSome:
          parent.myDef.genericDecl = some(mkAst(astGenericList))
        parent.myDef.genericDecl.get().myGenericList.mySeq.add(
          result.ast
        )
      of astStruct:
        #if parent.myStruct.genericDecl == nil:
        if not parent.myStruct.genericDecl.isSome:
          parent.myStruct.genericDecl = some(mkAst(astGenericList))
        parent.myStruct.genericDecl.get().myGenericList.mySeq.add(
          result.ast
        )
      else:
        doAssert(
          false,
          "eek! " & $parent.kind & " " & $self.lexMain
        )

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
    var tempType = self.parseTypeWithoutOptPreKwVar(chk=false).ast
    var myIdent = result.ast
    result.ast = mkAst(astGenericNamedArgImpl)
    result.ast.myGenericNamedArgImpl.ident = myIdent
    result.ast.myGenericNamedArgImpl.type = tempType

    var myGenericList = self.parentTempSeq[^1]
    myGenericList.myGenericList.mySeq.add result.ast

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
  let haveNamed = self.subParseIdentAssign(chk=true)
  result = self.parseTypeWithoutOptPreKwVar(chk=true)
  if chk:
    #echo "testificate:" & $result.foundTok & " " & $haveNamed.foundTok1
    if not haveNamed.foundTok.isSome:
      return
    else:
      result.foundTok = none(TokKind)
      result.foundTok1 = none(TokKind)
    #echo "post:" & $result.foundTok & " " & $haveNamed.foundTok1
  else: # if not chk:
    #if not haveNamed.foundTok.isSome:
    result = self.parseTypeWithoutOptPreKwVar(chk=false)
    var myGenericList = self.parentTempSeq[^1]
    myGenericList.myGenericList.mySeq.add result.ast
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
      true
      #false
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
  result.ast = mkAst(astGenericList)
  self.parentTempSeq.add result.ast

  let haveNamed = self.subParseIdentAssign(chk=true)
  if not haveNamed.foundTok.isSome:
    self.parseGenericUnnamedImplList()
    #echo "full:" & $self.lexMain
    #if self.lexAndCheck(chk=true, tok=tokComma).isSome:
    if self.currTok.tok == tokComma:
      #echo "full have check:" & $self.lexMain
      #self.lex()
      #echo "full have check post:" & $self.lexMain
      #discard self.optParse(
      #  chk=false,
      #  selProc=spp parseGenericNamedImplList,
      #)
      self.parseGenericNamedImplList()
  else:
    self.parseGenericNamedImplList()

  result.ast = self.parentTempSeq.pop()


  #self.lexAndCheck(chk=false, tok=tokRBrace)
  self.lexAndExpect(tokRBracket)



proc subParseFuncArgDeclList(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseIdent)
  var myIdent = result.ast
  result.ast = mkAst(astVarEtcDeclMost)
  result.ast.myVarEtcDeclMost.ident = myIdent

  self.lexAndExpect(tokColon)
  result.ast.myVarEtcDeclMost.type = (
    self.parseTypeWithOptPreKwVar(chk=false).ast
  )
  self.parentTempSeq[^1].myDef.argDeclSeq.add result.ast
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
    #discard self.parseTypeWithOptPreKwVar(chk=false)
    var tempExpr = self.parseExpr(chk=false).ast
    var myIdent = result.ast
    result.ast = mkAst(astFuncNamedArgImpl)
    result.ast.myFuncNamedArgImpl.ident = myIdent
    result.ast.myFuncNamedArgImpl.expr = tempExpr

    var myFuncCall = self.parentTempSeq[^1]
    myFuncCall.myFuncCall.argImplSeq.add result.ast
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
  #result = self.parseTypeWithoutOptPreKwVar(chk=true)
  let haveNamed = self.subParseIdentAssign(chk=true)
  result = self.parseExpr(chk=true)
  if chk:
    if not haveNamed.foundTok.isSome:
      return
    else:
      result.foundTok = none(TokKind)
      result.foundTok1 = none(TokKind)
  else: # if not chk:
    result = self.parseExpr(chk=false)
    var myFuncCall = self.parentTempSeq[^1]
    myFuncCall.myFuncCall.argImplSeq.add result.ast

proc parseFuncUnnamedArgImplList(
  self: var Scone,
  #chk: bool,
) =
  discard self.loopSelParse(
    selProcSeq=sppSeq @[parseFuncUnnamedArgImplItem],
    sepTok=some(tokComma),
    haveOptEndSepTok=(
      true
      #false
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
    #if self.lexAndCheck(chk=true, tok=tokComma).isSome:
    if self.currTok.tok == tokComma:
      #self.lex()
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
  result = self.parseExprFuncCallPostGenericMain(chk=chk)
  #result = doChkSelParse(
  #  sppSeq @[
  #    parseExprFuncCallPostGenericMain,
  #    #parseExpr,
  #  ],
  #  none(HashSet[TokKind]),
  #)

proc subParseStmtList(
  self: var Scone,
  #chk: bool,
  stmtSeq: var seq[AstNode],
) =
  var myStmt = self.parseStmt(chk=true)
  while myStmt.foundTok.isSome:
    myStmt = self.parseStmt(chk=false)
    stmtSeq.add myStmt.ast
    #self.lexAndExpect(tokSemicolon)
    myStmt = self.parseStmt(chk=true)


proc parseFuncDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseFuncDecl(): begin: chk:" & $chk
  discard doChkTok(tokDef)
  #echo "parseFuncDecl(): post `tokDef`"
  result.ast = mkAst(astDef)
  result.ast.myDef.ident = self.parseIdent(chk=false).ast
  #echo "parseFuncDecl(): " & $self.currIdentStrSeq
  #discard self.subParseGenericDeclList(chk=false)

  self.parentTempSeq.add result.ast

  discard self.optParse(chk=false, selProc=spp subParseGenericDeclList)

  self.lexAndExpect(tokLParen)
  # args go here
  discard self.parseFuncArgDeclList(chk=false)
  self.lexAndExpect(tokRParen)
  self.lexAndExpect(tokFuncReturnTypePrefix)
  result.ast.myDef.returnType = (
    self.parseTypeWithOptPreKwVar(chk=false).ast
  )

  self.lexAndExpect(tokLBrace)
  # stmts go here
  #var myStmt = self.parseStmt(chk=true)
  #while myStmt.foundTok.isSome:
  #  myStmt = self.parseStmt(chk=false)
  #  result.ast.myDef.stmtSeq.add myStmt.ast
  #  self.lexAndExpect(tokSemicolon)
  #  myStmt = self.parseStmt(chk=true)
  self.subParseStmtList(stmtSeq=result.ast.myDef.stmtSeq)
  self.lexAndExpect(tokRBrace)

  self.lexAndExpect(tokSemicolon)
  result.ast = self.parentTempSeq.pop()
  self.astRoot.mySrcFile.funcDeclSeq.add result.ast

proc subParseStructDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseVarEtcDeclMost)
  self.parentTempSeq[^1].myStruct.fieldSeq.add result.ast

proc parseStructDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokStruct)
  result.ast = mkAst(astStruct)
  result.ast.myStruct.ident = self.parseIdent(chk=false).ast

  self.parentTempSeq.add result.ast

  discard self.optParse(chk=false, selProc=spp subParseGenericDeclList)

  self.lexAndExpect(tokLBrace)
  # fields go here
  #echo "test"
  discard self.loopSelParse(
    selProcSeq=(
      sppSeq @[
        #parseVarEtcDeclMost
        subParseStructDecl
      ]
    ),
    sepTok=some(tokSemicolon),
    haveForcedEndSepTok=true,
  )
  self.lexAndExpect(tokRBrace)
  self.lexAndExpect(tokSemicolon)
  result.ast = self.parentTempSeq.pop()
  self.astRoot.mySrcFile.structDeclSeq.add result.ast

proc parseModule(
  self: var Scone,
  chk: bool,
) =
  self.lexAndExpect(tokModule)
  #self.lexAndExpect(tokIdent)
  #var module = AstNode(
  #  lexMain: self.lexMain,
  #  parent: myAst,
  #  kind: astModule,
  #)
  #myAst.srcFileVal.module = module
  #defer: discard unstack()
  template tempModule(): untyped = 
    self.astRoot.mySrcFile.module

  tempModule = (
    #mkAst(astModule, some(self.astRoot))
    mkAst(astModule)
  )
  tempModule.myModule.ident = (
    self.parseIdent(chk=false).ast
  )
  #self.astRoot.mySrcFile.module = myAst
  #echo myAst.repr()
  #echo $myAst
  #discard unstack()
  #echo $myAst

  self.lexAndExpect(tokSemicolon)

  # AST stuff here
  #self.mkAst(
  #)

proc parseExprFuncCallPostIdent(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.optParse(
    chk=chk,
    selProc=parseGenericFullImplList,
  )
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
      #if result.foundTok.isSome:
      #  echo "result.foundTok.isSome:" & $result.foundTok.get() #$result.ast
      #  doAssert(
      #    not temp.foundTok.isSome,
      #    "eek! " & $result.foundTok.get() & " " & $temp.foundTok.get()
      #  )
      #  echo "parentTempSeq.len(): " & $self.parentTempSeq.len()
      #  echo $self.parentTempSeq[^1]
      #else:
      #  doAssert(
      #    temp.foundTok.isSome,
      #    "eek! " & $temp.foundTok.get()
      #  )
      #  echo "temp.foundTok.isSome:" & $temp.foundTok.get() #$temp.ast
      #echo "parentTempSeq.len(): " & $self.parentTempSeq.len()
      if self.parentTempSeq.len() > 0:
        #$echo self.parentTempSeq[^1]
        discard
      else:
        doAssert(
          false,
          "eek! " & $result.ast
        )
      if result.foundTok.isSome:
        self.parentTempSeq[^1].myFuncCall.genericImpl = result.ast
      else:
        self.parentTempSeq[^1].myFuncCall.genericImpl = (
          mkAst(astGenericList)
        )
      result = self.parseExprFuncCallPostGeneric(chk=false)
    else:
      #echo "parseExprFuncCallPostIdent(): lexAndExpect:"
      #echo $self.astRoot
      self.lexAndExpect(tokSet=result.tokSet)
    #discard self.optParseThenExpectTokSeq(
    #  chk=chk,
    #  selProc=parseExprFuncCallPostGeneric,
    #  postTokSeq=(@[tokRParen]),
    #)

proc parseExprOpenarrayLit(
  self: var Scone,
  chk: bool,
): SppResult =
  discard
  
proc parseExprIdentOrFuncCall(
  self: var Scone,
  chk: bool,
): SppResult =
  proc subParseExprIdentOrFuncCall(
    self: var Scone,
    chk: bool,
  ): SppResult =
    result = doChkSpp(parseIdent)
    self.parentTempSeq.add mkAst(astFuncCall)
    let temp = self.optParse(
      chk=false,
      selProc=spp parseExprFuncCallPostIdent,
    )
    if temp.foundTok.isSome:
      let myIdent = result.ast 
      result.ast = self.parentTempSeq[^1]
      result.ast.myFuncCall.ident = myIdent
    discard self.parentTempSeq.pop()
  proc subParseExprBuiltinTypeCast(
    self: var Scone,
    chk: bool,
  ): SppResult =
    result = doChkSpp(parseTypeBuiltinWithoutOptPreKwVar)
    #self.parentTempSeq.add mkAst(
    #self.parentTempSeq

  #proc subParseExprMkOpenarrayCall(
  #  self: var Scone,
  #  chk: bool
  #): SppResult =
  #  discard

  result = doChkSelParse(
    sppSeq @[
      subParseExprIdentOrFuncCall,
      subParseExprBuiltinTypeCast,
      parseExprOpenarrayLit,
    ],
    none(HashSet[TokKind])
  )

proc parseExprIdentOrFuncCallPostDot(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseIdent)
  self.parentTempSeq.add mkAst(astFuncCall)
  let temp = self.optParse(
    chk=false,
    selProc=spp parseExprFuncCallPostIdent,
  )
  if temp.foundTok.isSome:
    let myIdent = result.ast 
    result.ast = self.parentTempSeq[^1]
    result.ast.myFuncCall.ident = myIdent
    #echo "testificate:" & $result.ast
  discard self.parentTempSeq.pop()

# BEGIN: Old version of `parseExprBinopFuncCall`
#proc parseExprBinopFuncCall(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  result = doChkSpp(parseIdent)
#  #var myIdent = result.ast
#
#  #var parent = self.parentTempSeq[^1]
#  #case parent.kind
#
#  self.parentTempSeq.add mkAst(astFuncCall)
#  self.parentTempSeq[^1].myFuncCall.ident = result.ast
#
#  discard self.optParse(
#    chk=false,
#    selProc=spp parseGenericFullImplList,
#  )
#
#  var myExpr = (
#    #self.parseExpr(chk=false)
#    #self.parseExprFieldArrEtc(chk=false)
#    self.parseExprLowestNonOp(chk=false)
#  )
#  result.ast = self.parentTempSeq.pop()
#  result.ast.myFuncCall.argImplSeq.add myExpr.ast
#  #echo "test:" & $result.ast
# END: Old version of `parseExprBinopFuncCall`

#proc subParseExprBinopFuncCall(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  result = doChkSpp(parseIdent)
#  #var myIdent = result.ast
#
#  #var parent = self.parentTempSeq[^1]
#  #case parent.kind
#
#  self.parentTempSeq.add mkAst(astFuncCall)
#  self.parentTempSeq[^1].myFuncCall.ident = result.ast
#
#  discard self.optParse(
#    chk=false,
#    selProc=spp parseGenericFullImplList,
#  )
#
#  var myExpr = (
#    #self.parseExpr(chk=false)
#    #self.parseExprFieldArrEtc(chk=false)
#    #self.parseExprLowestNonOp(chk=false)
#    self.parseExprUnary(chk=false)
#  )
#  result.ast = self.parentTempSeq.pop()
#  result.ast.myFuncCall.argImplSeq.add myExpr.ast
#  #echo "test:" & $result.ast


#proc parseExprBinopFuncCall(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  result = doChkSpp(parseExprUnary)
#
#  #self.parentTempSeq.add result.ast
#
#  #var loopParseOutp = self.loopSelParse(
#  #  selProcSeq=(
#  #    sppSeq @[subParseExprBinopFuncCall]
#  #  ),
#  #  sepTok=none(TokKind),
#  #)
#  #if loopParseOutp.ast != nil:
#  #  case loopParseOutp.ast.kind:
#  #  of astFuncCall:
#  #    discard
#  #  else:
#  #    doAssert(
#  #      false,
#  #      "eek! loopParseOutp.ast:" & $loopParseOutp.ast
#  #    )
#  #  #var myUnary = result.ast
#  #  #var myExpr = 
#  #  #result.ast = loopParseOutp
#  #discard self.parentTempSeq.pop()
#
#  #if not chk:
#  var haveIdent = self.parseIdent(chk=true)
#  #var atFirstIter: bool = true
#  #var myExpr = result.ast
#  #var firstCall: AstNode = nil
#  #var lastFuncCall: AstNode = nil
#  while haveIdent.foundTok.isSome:
#    #result.ast = mkAst(astFuncCall)
#    haveIdent = self.parseIdent(chk=false)
#    var myExpr: AstNode = result.ast
#    #if atFirstIter:
#    #  #firstExpr = result.ast
#    #  #firstCall = mkAst(astFuncCall)
#    #  atFirstIter = false
#    #else:
#    #  #result.ast.myFuncCall.argImplSeq.add restul
#    result.ast = mkAst(astFuncCall)
#    result.ast.myFuncCall.argImplSeq.add myExpr
#    result.ast.myFuncCall.ident = haveIdent.ast
#    self.parentTempSeq.add result.ast
#    discard self.optParse(
#      chk=false,
#      selProc=spp parseGenericFullImplList,
#    )
#    result.ast = self.parentTempSeq.pop()
#    result.ast.myFuncCall.argImplSeq.add self.parseExprUnary(chk=false).ast
#
#    #lastFuncCall = result.ast
#    haveIdent = self.parseIdent(chk=true)



  #result.ast = self.parentTempSeq.pop()

  #result = doChkSpp(parseIdent)
  #var myIdent = result.ast

  #var parent = self.parentTempSeq[^1]
  #case parent.kind

  #self.parentTempSeq.add mkAst(astFuncCall)
  #self.parentTempSeq[^1].myFuncCall.ident = result.ast

  #discard self.optParse(
  #  chk=false,
  #  selProc=spp parseGenericFullImplList,
  #)

  #var myExpr = (
  #  #self.parseExpr(chk=false)
  #  #self.parseExprFieldArrEtc(chk=false)
  #  self.parseExprLowestNonOp(chk=false)
  #)
  #result.ast = self.parentTempSeq.pop()
  #result.ast.myFuncCall.argImplSeq.add myExpr.ast
  ##echo "test:" & $result.ast

#proc parseExprFuncCall(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  result = doChkSpp(parseIdent)
#  #var left = self.parentTempSeq[^1]
#  #parentAst.myFuncCall.ident = result.ast
#  let myIdent = result.ast
#
#  result.ast = mkAst(astFuncCall)
#  result.ast.myFuncCall.ident = myIdent
#
#  self.parentTempSeq.add(result.ast)
#  discard self.parseExprFuncCallPostIdent(chk=false)
#  discard self.parentTempSeq.pop()
#  #if temp.ast != nil:
  
proc parseU64Lit(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokU64Lit)
  result.ast = mkAst(astU64Lit)
  #defer: discard unstack()
  result.ast.myU64Lit.u64Val = self.currTok.optU64.get()
  #echo "parseU64Lit: " & $result.ast

  ##self.parentTempSeq.add(result.ast)
  #var temp = self.optParse(
  #  chk=false,
  #  selProc=spp parseExprFuncCall,
  #)
  ##self.parentTempSeq.setLen(self.parentTempSeq.len() - 1)
  ##echo temp.ast.repr()
  #if temp.ast != nil:
  #  #var methodAst = mkAst(astMethodCall)
  #  #methodAst.myMethodCall.obj = result.ast
  #  #methodAst.myMethodCall.funcCall = temp.ast
  #  #result.ast = methodAst
  #  let myTemp = result.ast
  #  result.ast = temp.ast
  #  result.ast.myFuncCall.argImplSeq.insert(myTemp)

proc subParseParenExpr(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLParen)
  result = self.parseExpr(chk=false)
  self.lexAndExpect(tokRParen)

  #self.parentTempSeq.add(result.ast)
  #var temp = self.optParse(
  #  chk=false,
  #  selProc=spp parseExprFuncCall,
  #)
  #self.parentTempSeq.setLen(self.parentTempSeq.len() - 1)

  #if temp.ast != nil:
  #  #var methodAst = mkAst(astMethodCall)
  #  #methodAst.myMethodCall.obj = result.ast
  #  #methodAst.myMethodCall.funcCall = temp.ast
  #  #result.ast = methodAst
  #  let myTemp = result.ast
  #  result.ast = temp.ast
  #  result.ast.myFuncCall.argImplSeq.insert(myTemp)

proc parseExprLowestNonOp(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseExprLowestNonOp: pre: " & "chk:" & $chk & " " & $self.lexMain
  result = doChkSelParse(
    sppSeq @[
      parseExprIdentOrFuncCall,
      parseU64Lit,
      subParseParenExpr,
    ],
    none(HashSet[TokKind]),
  )
  #echo "parseExprLowestNonOp: post: " & "chk:" & $chk & " " & $self.lexMain
  if not chk:
    #echo "parseExprLowestNonOp: not chk: " & $result.ast
    discard

proc subLoopSelParseExprBinop(
  self: var Scone,
  chk: bool,
  tokSet: HashSet[TokKind],
  someSppRet: var SppResult,
  subExprParseFunc: SelParseProc,
): SppResult =
  if chk:
    discard doChkTokSetWithChk(argChk=true, tokSet)
  else: # if not chk:
    #while result.foundTok.isSome:
    #echo $result
    result.foundTok = self.lexAndCheck(chk=true, tokSet)
    if result.foundTok.isSome:
      #echo "test"
      self.lex()
      while true:
        result.ast = mkAst(astBinop)
        result.ast.myBinop.kind = tokToBinop(result.foundTok.get()).get()
        result.ast.myBinop.left = someSppRet.ast
        #defer: discard unstack()
        result.ast.myBinop.right = (
          subExprParseFunc(self=self, chk=false).ast
        )
        someSppRet.ast = result.ast
        let tempFoundTok = self.lexAndCheck(chk=true, tokSet)
        if tempFoundTok.isSome:
          result.foundTok = tempFoundTok
          self.lex()
        else:
          break
      #echo "testificate: " & $result.ast
#proc subLoopSelParseExprBinop(
#  self: var Scone,
#  chk: bool,
#  #tokSet: HashSet[TokKind],
#  #selProcSet: OrderedSet[SelParseProc],
#  selProc: SelParseProc,
#  someSppRet: var SppResult,
#  subExprParseFunc: SelParseProc,
#): SppResult =
#  if chk:
#    #discard doChkTokSetWithChk(argChk=true, tokSet)
#    result = doChkSpp(selProc)
#  else: # if not chk:
#    #while result.foundTok.isSome:
#    #echo $result
#    #result.foundTok = self.lexAndCheck(chk=true, tokSet)
#    result = selProc(self=self, chk=true)
#    if result.foundTok.isSome:
#      #echo "test"
#      #self.lex()
#      result = selProc(self=self, chk=false)
#      while true:
#        result.ast = mkAst(astBinop)
#        result.ast.myBinop.kind = tokToBinop(result.foundTok.get()).get()
#        result.ast.myBinop.left = someSppRet.ast
#        #defer: discard unstack()
#        result.ast.myBinop.right = (
#          subExprParseFunc(self=self, chk=false).ast
#        )
#        someSppRet.ast = result.ast
#        let tempFoundTok = self.lexAndCheck(chk=true, tokSet)
#        if tempFoundTok.isSome:
#          result.foundTok = tempFoundTok
#          self.lex()
#        else:
#          break

proc subLoopSelParseExprBinop(
  self: var Scone,
  chk: bool,
  tokSeq: seq[TokKind],
  someSppRet: var SppResult,
  subExprParseFunc: SelParseProc,
): SppResult =
  result = self.subLoopSelParseExprBinop(
    chk=chk,
    tokSet=toHashSet(tokSeq),
    someSppRet=someSppRet,
    subExprParseFunc=subExprParseFunc,
  )
proc subLoopSelParseExprBinop(
  self: var Scone,
  chk: bool,
  tok: TokKind,
  someSppRet: var SppResult,
  subExprParseFunc: SelParseProc,
): SppResult =
  result = self.subLoopSelParseExprBinop(
    chk=chk,
    tokSet=toHashSet([tok]),
    someSppRet=someSppRet,
    subExprParseFunc=subExprParseFunc,
  )

proc parseExprPrefixUnary(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTokSet(toHashSet([
    tokPlus, 
    tokMinus, tokLogicNot, tokBitInvert,
    tokAddr
  ]))

proc parseExprSuffixFieldMethodAccessDotExpr(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo (
  #  (
  #    "parseExprSuffixFieldMethodAccessDotExpr(): pre tokDot: "
  #  ) & (
  #    $chk & " " & $self.lexMain
  #  )
  #)
  discard doChkTok(tokDot)
  #echo (
  #  (
  #    "parseExprSuffixFieldMethodAccessDotExpr(): post tokDot: "
  #  ) & (
  #    $chk & " " & $self.lexMain
  #  )
  #)
  #self.parseExprIdentOrFuncCall(chk=false)
  var temp = self.parseExprIdentOrFuncCallPostDot(chk=false)

  case temp.ast.kind:
  of astIdent:
    result.ast = mkAst(astDot)
    result.ast.myDot.right = temp.ast
  of astFuncCall:
    result.ast = temp.ast
  else:
    doAssert(
      false,
      "eek! " & temp.ast.toStr(0)
    )
proc parseExprSuffixFieldMethodAccess(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseExprSuffixFieldMethodAccess: before:"
  result = doChkSelParse(
    sppSeq @[
      #parseExprBinopFuncCall,
      parseExprSuffixFieldMethodAccessDotExpr,
    ],
    none(HashSet[TokKind]),
  )
  #echo "parseExprSuffixFieldMethodAccess: after:"
  #if result.ast != nil:
  #  echo $result.ast

proc parseExprSuffixDeref(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokDeref)
  result.ast = mkAst(astDeref)
  result.ast.myDeref.obj = self.parentTempSeq.pop()
  self.parentTempSeq.add result.ast

#proc parseExprSuffixArray(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  discard doChkTok(tokLBrace)
#  discard self.parseExpr(chk=false)
#  discard doChkTok(tokRBrace)

proc parseExprFieldArrEtcChoice(
  self: var Scone,
  chk: bool,
): SppResult =
  #result = self.selParse(
  #)
  #echo "parseExprFieldArrEtcChoice: before selParse"
  let temp = self.selParse(
    chk=true,
    selProcSeq=(
      sppSeq @[
        parseExprSuffixFieldMethodAccess,
        parseExprSuffixDeref,
        #parseExprSuffixArray,
      ]
    )
  )
  #echo "parseExprFieldArrEtcChoice: after selParse: " & $temp
  result = temp
  if chk:
    #result = temp
    discard
  else: # if not chk:
    doAssert(
      temp.foundTok.isSome,
      "eek! " & $temp
    )
    if temp.foundSelProc.get() == parseExprSuffixFieldMethodAccess:
      var myTemp = temp.foundSelProc.get()(self=self, chk=false)
      case myTemp.ast.kind:
      of astFuncCall:
        var myPrev = self.parentTempSeq.pop()
        myTemp.ast.myFuncCall.argImplSeq.insert(myPrev)
        self.parentTempSeq.add myTemp.ast
      #of astIdent:
      #  discard
      of astDot:
        var myPrev = self.parentTempSeq.pop()
        myTemp.ast.myDot.left = myPrev
        self.parentTempSeq.add myTemp.ast
      else:
        doAssert(
          false,
          "eek! " & myTemp.ast.toStr(0)
        )
    elif temp.foundSelProc.get() == parseExprSuffixDeref:
      discard temp.foundSelProc.get()(self=self, chk=false)
    else:
      doAssert(
        false,
        "eek! " & $temp
      )

proc parseExprFieldArrEtc(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprLowestNonOp)
  let temp = self.parseExprFieldArrEtcChoice(chk=true)
  if temp.foundTok.isSome:
    #var lowestNonOp = result.ast
    #result.ast = mkAst(astFuncCall)
    self.parentTempSeq.add result.ast
    discard self.loopSelParse(
      selProcSeq=(
        sppSeq @[parseExprFieldArrEtcChoice]
      ),
    )
    #result.ast.myFuncCall.argImplSeq.insert(lowestNonOp)
    result.ast = self.parentTempSeq.pop()
    #self.parentTempSeq.setLen(self.parentTempSeq.len() - 1)

#proc parseExprOptPrefixUnaryMain(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  #result = self.optParseThenExpectSpp(
#  #  chk=chk,
#  #  selProc=parseExprPrefixUnary,
#  #  postSelProc=parseExprFieldArrEtc,
#  #)
#  result = self.optParse(
#    chk=chk,
#    selProc=spp parseExprPrefixUnary,
#  )

#proc parseExprAddrLhsMain(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  discard doChkTok(tokAddr)
#  result.ast = mkAst(astAddr)
#  result.ast.myAddr.obj = self.parseExprLhsMain(chk=false).ast

proc parseExprUnary(
  self: var Scone,
  chk: bool,
): SppResult =
  #echo "parseExprUnary: lexMain: pre: " & $self.lexMain
  #echo "parseExprUnary: lexMain: post: " & $self.lexMain
  #result = self.selParse(
  #  chk=false,
  #  sppSeq @[
  #    parseExprOptPrefixUnaryMain
  #  ],
  #)

  #result = doChkSelParse(
  #  sppSeq @[
  #    parseExprOptPrefixUnaryMain
  #  ],
  #  none(HashSet[TokKind]),
  #)
  result = self.optParse(
    chk=chk,
    selProc=spp parseExprPrefixUnary,
  )
  let temp = self.parseExprFieldArrEtc(
    chk=true
  )
  result.tokSet = result.tokSet.union(temp.tokSet)
  if chk:
    if result.foundTok.isSome:
      return
    else:
      result.foundTok = self.lexAndCheck(chk=true, tokSet=result.tokSet)
  else: # if not chk:
    if result.foundTok.isSome or temp.foundTok.isSome:
      if result.foundTok.isSome:
        result.ast = mkAst(astUnop)
        result.ast.myUnop.kind = tokToUnop(result.foundTok.get()).get()
        result.ast.myUnop.obj = self.parseExprFieldArrEtc(chk=false).ast
      else:
        result.ast = self.parseExprFieldArrEtc(chk=false).ast
    else:
      self.lexAndExpect(tokSet=result.tokSet)


proc parseExprMulDivMod(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(
    parseExprUnary
    #parseExprBinopFuncCall
  )
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tokSeq=(@[tokMul, tokDiv, tokMod]),
    someSppRet=result,
    subExprParseFunc=(
      parseExprUnary
      #parseExprBinopFuncCall
    ),
  )

proc parseExprAddSub(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprMulDivMod)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tokSeq=(@[tokPlus, tokMinus]),
    someSppRet=result,
    subExprParseFunc=parseExprMulDivMod,
  )

proc parseExprBitShift(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprAddSub)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tokSeq=(@[tokBitShl, tokBitShr]),
    someSppRet=result,
    subExprParseFunc=parseExprAddSub,
  )

proc parseExprCmpIneq(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprBitShift)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tokSeq=(@[tokCmpLt, tokCmpLe, tokCmpGt, tokCmpGe]),
    someSppRet=result,
    subExprParseFunc=parseExprBitShift,
  )

proc parseExprCmpEqNe(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprCmpIneq)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tokSeq=(@[tokCmpEq, tokCmpNe]),
    someSppRet=result,
    subExprParseFunc=parseExprCmpIneq,
  )

proc parseExprBitAnd(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprCmpEqNe)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tok=tokBitAnd,
    someSppRet=result,
    subExprParseFunc=parseExprCmpEqNe,
  )

proc parseExprBitXor(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprBitAnd)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tok=tokBitXor,
    someSppRet=result,
    subExprParseFunc=parseExprBitAnd,
  )

proc parseExprBitOr(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprBitXor)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tok=tokBitOr,
    someSppRet=result,
    subExprParseFunc=parseExprBitXor,
  )

proc parseExprLogicAnd(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprBitOr)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tok=tokLogicAnd,
    someSppRet=result,
    subExprParseFunc=parseExprBitOr,
  )

proc parseExprLogicOr(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprLogicAnd)
  discard self.subLoopSelParseExprBinop(
    chk=false,
    tok=tokLogicOr,
    someSppRet=result,
    subExprParseFunc=parseExprLogicAnd,
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


proc parseStmtVarDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokVar)
  result.ast = mkAst(astVar)
  result.ast.myVar.child = self.parseVarEtcDeclMost(chk=false).ast
  result.ast.myVar.optExpr = none(AstNode)
  if self.lexAndCheck(chk=true, tok=tokAssign).isSome:
    self.lex()
    result.ast.myVar.optExpr = some(self.parseExpr(chk=false).ast)
  self.lexAndExpect(tokSemicolon)

proc parseStmtConstDecl(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokConst)
  result.ast = mkAst(astConst)
  result.ast.myConst.child = self.parseVarEtcDeclMost(chk=false).ast
  discard self.lexAndCheck(chk=false, tok=tokAssign)
  result.ast.myConst.expr = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokSemicolon)

proc parseStmtBreak(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokBreak)
  self.lexAndExpect(tokSemicolon)
proc parseStmtContinue(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokContinue)
  self.lexAndExpect(tokSemicolon)

proc parseStmtFor(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokFor)
  result.ast = mkAst(astFor)
  result.ast.myFor.ident = self.parseIdent(chk=false).ast
  self.lexAndExpect(tokIn)
  result.ast.myFor.exprPre = self.parseExpr(chk=false).ast
  self.lex()
  if self.currTok.tok == tokTo:
    result.ast.myFor.isUntil = false
  elif self.currTok.tok == tokUntil:
    result.ast.myFor.isUntil = true
  else:
    self.expect(toHashSet([tokTo, tokUntil]))
  result.ast.myFor.exprPost = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myFor.stmtSeq)
  self.lexAndExpect(tokRBrace)
proc parseStmtWhile(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokWhile)
  result.ast = mkAst(astWhile)
  result.ast.myWhile.expr = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myWhile.stmtSeq)
  self.lexAndExpect(tokRBrace)

proc parseStmtElse(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokElse)
  result.ast = mkAst(astElse)
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myElse.stmtSeq)
  self.lexAndExpect(tokRBrace)

proc parseStmtElif(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokElif)
  result.ast = mkAst(astElif)
  result.ast.myElif.expr = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myElif.stmtSeq)
  self.lexAndExpect(tokRBrace)

proc parseStmtIf(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokIf)
  result.ast = mkAst(astIf)
  result.ast.myIf.expr = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myIf.stmtSeq)
  self.lexAndExpect(tokRBrace)
  var myElif = self.parseStmtElif(chk=true)
  while myElif.foundTok.isSome:
    myElif = self.parseStmtElif(chk=false)
    result.ast.myIf.elifSeq.add myElif.ast
    myElif = self.parseStmtElif(chk=true)
  result.ast.myIf.optElse = none(AstNode)
  var myElse = self.parseStmtElse(chk=true)
  if myElse.foundTok.isSome:
    result.ast.myIf.optElse = some(self.parseStmtElse(chk=false).ast)

proc parseStmtDefault(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokDefault)
  result.ast = mkAst(astDefault)
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myDefault.stmtSeq)
  self.lexAndExpect(tokRBrace)

proc parseStmtCase(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokCase)
  result.ast = mkAst(astCase)
  result.ast.myCase.expr = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myCase.stmtSeq)
  self.lexAndExpect(tokRBrace)

proc parseStmtSwitch(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokSwitch)
  result.ast = mkAst(astSwitch)
  result.ast.mySwitch.expr = self.parseExpr(chk=false).ast
  self.lexAndExpect(tokLBrace)

  var myCase = self.parseStmtCase(chk=true)
  while myCase.foundTok.isSome:
    myCase = self.parseStmtCase(chk=false)
    result.ast.mySwitch.caseSeq.add myCase.ast
    myCase = self.parseStmtCase(chk=true)

  result.ast.mySwitch.optDefault = none(AstNode)
  var myDefault = self.parseStmtDefault(chk=true)
  if myDefault.foundTok.isSome:
    result.ast.mySwitch.optDefault = (
      some(self.parseStmtDefault(chk=false).ast)
    )
  self.lexAndExpect(tokRBrace)

proc parseStmtScope(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokScope)
  result.ast = mkAst(astScope)
  self.lexAndExpect(tokLBrace)
  self.subParseStmtList(result.ast.myScope.stmtSeq)
  self.lexAndExpect(tokRBrace)

proc parseStmtReturn(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokReturn)
  result.ast = mkAst(astReturn)
  result.ast.myReturn.optExpr = none(AstNode)
  var myExpr = self.parseExpr(chk=true)
  if myExpr.foundTok.isSome:
    result.ast.myReturn.optExpr = some(self.parseExpr(chk=false).ast)
  self.lexAndExpect(tokSemicolon)


proc subParseExprLhsTokAddr(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokAddr)

proc subParseExprLhsParenExprLhs(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTok(tokLParen)
  result = self.parseExprLhs(chk=false)
  self.lexAndExpect(tokRParen)

proc parseExprLhsLowestNonOp(
  self: var Scone,
  chk: bool,
): SppResult =
  result = self.optParse(
    chk=chk,
    selProc=subParseExprLhsTokAddr
  )
  let temp = self.selParse(
    chk=true,
    sppSeq @[
      parseExprIdentOrFuncCall,
      subParseExprLhsParenExprLhs,
    ],
  )
  result.tokSet = result.tokSet.union(temp.tokSet)
  if chk:
    if result.foundTok.isSome:
      return
    else:
      result.foundTok = self.lexAndCheck(chk=true, tokSet=result.tokSet)
  else:
    if result.foundTok.isSome or temp.foundTok.isSome:
      if result.foundTok.isSome:
        result.ast = mkAst(astUnop)
        result.ast.myUnop.kind = unopAddr
        result.ast.myUnop.obj = temp.foundSelProc.get()(
          self=self,
          chk=false,
        ).ast
      else:
        result.ast = temp.foundSelProc.get()(
          self=self,
          chk=false,
        ).ast
    else:
      self.lexAndExpect(tokSet=result.tokSet)

proc parseExprLhs(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprLhsLowestNonOp)
  let temp = self.parseExprFieldArrEtcChoice(chk=true)
  if temp.foundTok.isSome:
    self.parentTempSeq.add result.ast
    discard self.loopSelParse(
      selProcSeq=(
        sppSeq @[parseExprFieldArrEtcChoice]
      )
    )
    result.ast = self.parentTempSeq.pop()
  
#proc parseExprLhs(
#  self: var Scone,
#  chk: bool,
#): SppResult =
#  #result = self.optParseThenExpectSpp(
#  #  chk=chk,
#  #  selProc=parseExprPrefixUnary,
#  #  postSelProc=parseExprFieldArrEtc,
#  #)
#  discard

proc parseAssignOp(
  self: var Scone,
  chk: bool,
): SppResult =
  discard doChkTokSet(toHashSet([
    tokAssign,
    tokAssignPlus, tokAssignMinus,
    tokAssignMul, tokAssignDiv, tokAssignMod,
    tokAssignBitAnd, tokAssignBitOr, tokAssignBitXor,
    tokAssignBitShl, tokAssignBitShr,
  ]))

proc parseStmtCallOrAssignEtc(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSpp(parseExprLhs)
  var temp = self.parseAssignOp(chk=true)
  if temp.foundTok.isSome:
    var myExprLhs = result.ast
    temp = self.parseAssignOp(chk=false)
    result.ast = mkAst(astAssignEtc)
    result.ast.myAssignEtc.kind = (
      tokToAssignEtc(temp.foundTok.get()).get()
    )
    result.ast.myAssignEtc.left = myExprLhs
    result.ast.myAssignEtc.right = self.parseExpr(chk=false).ast
  else:
    var myExprLhs = result.ast
    result.ast = mkAst(astStmtExprLhs)
    result.ast.myStmtExprLhs.expr = myExprLhs
  self.lexAndExpect(tokSemicolon)

proc parseStmt(
  self: var Scone,
  chk: bool,
): SppResult =
  result = doChkSelParse(
    sppSeq @[
      parseStmtVarDecl, parseStmtConstDecl,
      parseStmtBreak, parseStmtContinue,
      parseStmtFor, parseStmtWhile,
      parseStmtIf,
      parseStmtSwitch,
      parseStmtScope,
      parseStmtReturn,
      parseStmtCallOrAssignEtc,
    ],
    none(HashSet[TokKind]),
  )

proc parseSrcFile(
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
  if temp.foundTok.isSome:
    #echo "test"
    discard self.loopSelParse(
      mySppSeq
    )
  else:
    # this *may* be an error
    self.lexAndExpect(
      temp.tokSet.union(toHashSet([tokEof]))
    )

  self.lexAndExpect(tokEof)
  #self.astRoot = self.parseExpr(chk=false).ast
  #echo $self.astRoot.kind
  echo $self.astRoot
  #echo self.astRoot.toRepr()
  #echo self.astRoot.repr()
  #let temp = self.loopSelParse()
  #if not temp.foundTok.isSome:
  #  discard

proc doPassParse*(
  self: var Scone
) =
  self.parseSrcFile()
