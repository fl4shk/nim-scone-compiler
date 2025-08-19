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
  result.foundTok = none(TokKind)
  let hiddenMyTok = self.lexAndCheck(chk=chk, argTokSet)
  if chk:
    if hiddenMyTok.isSome:
      #return some(SppResultMain(
      #  tokSet: argTokSet,
      #  foundTok: hiddenMyTok.get(),
      #))
      return SppResult(
        tokSet: argTokSet,
        foundTok: hiddenMyTok,
      )
    else:
      #return none(SppResultMain)
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

proc optParse(
  self: var Scone,
  chk: bool,
  selProc: SelParseProc,
): SppResult =
  result = SppResult(foundTok: none(TokKind))
  self.stackSavedIlp()
  let sppRet = selProc(self=self, chk=true)
  if sppRet.foundTok.isSome:
    self.unstackSavedIlp()
    if not chk:
      return selProc(self=self, chk=false)
  self.unstackSavedIlp()


proc selParse(
  self: var Scone,
  selProcSet: HashSet[SelParseProc],
): (SelParseProc, SppResult) =
  #for myProc in selProcSet:
  #  if myProc[](self, true):
  #    return myProc
  #for idx in 0 ..< selProcSet.len():
  for selProc in selProcSet:
    self.stackSavedIlp()
    let sppRet = selProc(self=self, chk=true)
    result[1].tokSet = result[1].tokSet.union(sppRet.tokSet)

    if sppRet.foundTok.isSome:
      self.unstackSavedIlp()
      return (selProc, sppRet)
    self.unstackSavedIlp()
    
  #echo "selParse(): returning `none`"
  result[1].foundTok = none(TokKind)

proc selParse(
  self: var Scone,
  selProcSeq: seq[SelParseProc]
): (SelParseProc, SppResult) =
  result = self.selParse(selProcSet=toHashSet(selProcSeq))
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
) =
  var mySpp = self.selParse(selProcSet)
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
    mySpp = self.selParse(selProcSet)

  if not didBreak and haveOptEndSepTok:
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
) =
  #var tempSelProcSeq: seq[SelParseProc]
  #for selProc in selProcArr:
  #  tempSelProcSeq.add sppSeq(selProc)
  self.loopSelParse(
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

  self.loopSelParse(
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
  result.foundTok = none(TokKind)

  let mySpp = self.selParse(
    sppSeq @[
      parseTypeBuiltinScalar,
      parseTypeToResolve,
    ]
  )
  if chk:
    #echo "parseTypeMain: chk: " & $mySpp
    if mySpp[1].foundTok.isSome:
      #return some(mySpp.get()[1])
      return mySpp[1]
  elif mySpp[1].foundTok.isSome:
    #echo "parseTypeMain: not chk: " & $mySpp
    #mySpp.get()(self=self, chk=chk)
    #let temp = mySpp.get()
    discard mySpp[0](self=self, chk=chk)
  else:
    # this will be an error
    self.lexAndExpect(tokSet=mySpp[1].tokSet)

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

proc parseGenericDeclList(
  self: var Scone,
  #chk: bool,
) =
  self.loopSelParse(
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
  self.loopSelParse(
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
  #echo "haveNoArgs: " & $haveNoArgs
  if haveNoArgs.isSome:
    #echo "haveNoArgs.isSome: begin: " & $chk & " " & $self.currTok
    discard doChkTok(tokResult)
    discard self.lexAndCheck(chk=false, tok=tokColon)
    #echo "haveNoArgs.isSome: post tokResult: " & $self.currTok
    discard self.parseTypeWithOptPreKwVar(chk=false)
    #echo (
    #  "haveNoArgs.isSome: post parseTypeWithOptPreKwVar: " & $self.currTok
    #)
  else:
    #echo "not haveNoArgs.isSome: begin"
    discard doChkSpp(subParseFuncArgDeclList)
    #echo "not haveNoArgs.isSome: post doChkSpp"

    self.loopSelParse(
      selProcSeq=(
        sppSeq @[subParseFuncArgDeclList]
      ),
      sepTok=some(tokComma),
      haveOptEndSepTok=true,
    )
    discard self.lexAndCheck(chk=false, tok=tokResult)
    discard self.lexAndCheck(chk=false, tok=tokColon)
    discard self.parseTypeWithOptPreKwVar(chk=false)


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

proc parseExpr(
  self: var Scone,
  chk: bool,
): SppResult =
  result.foundTok = none(TokKind)

proc parseSrcFile*(
  self: var Scone
) =
  self.parseModule(chk=false)

  self.loopSelParse(sppSeq @[
    parseFuncDecl,
    parseStructDecl,
  ])
