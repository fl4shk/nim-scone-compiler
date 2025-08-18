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
  SelParseProc = proc(
    self: var Scone,
    chk: bool,
  ): bool
#template SelParseProc(): untyped = 
#  proc(self: var Scone, chk: bool): bool
template spp(
  someProc: untyped
): untyped =
  SelParseProc(someProc)

template doChkTok(
  tokOrTokSet: untyped,
): untyped =
  result = false
  let hiddenMyTok = self.lexAndCheck(chk, tokOrTokSet)
  if chk:
    if hiddenMyTok.isSome:
      return true
    else:
      return false
  hiddenMyTok


proc selParse(
  self: var Scone,
  selProcSet: HashSet[SelParseProc],
): Option[SelParseProc] =
  #for myProc in selProcSet:
  #  if myProc[](self, true):
  #    return myProc
  #for idx in 0 ..< selProcSet.len():
  #echo "selParse(): begin"
  for selProc in selProcSet:
    #echo "selParse(): loop begin: " #& $selProc
    self.stackSavedIlp()
    if selProc(self=self, chk=true):
      #echo "selParse(): found something: " & $self.currTok
      self.unstackSavedIlp()
      return some(selProc)
    self.unstackSavedIlp()
    #echo "selParse(): loop end"
    
  #echo "selParse(): returning `none`"
  return none(SelParseProc)

#template doCheckSpp(
#  selProc: untyped,
#): untyped =
#  result = false

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
  selProcSet: HashSet[SelParseProc],
  sepTok: Option[TokKind]=none(TokKind),
  #endTok: Option[TokKind]=none(TokKind),
) =
  var mySpp = self.selParse(selProcSet)
  while mySpp.isSome:
    #echo "loopSelParse(): begin: " & $sepTok
    discard mySpp.get()(self=self, chk=false)
    #echo "post mySpp.get()"
    if sepTok.isSome:
      #self.lexAndExpect(sepTok.get())
      self.stackSavedIlp()
      self.lex()
      if self.currTok.tok != sepTok.get():
        self.unstackSavedIlp()
        #echo "loopSelParse(): break"
        break
      self.unstackSavedIlp()
    #echo "loopSelParse(): end"
    mySpp = self.selParse(selProcSet)

#proc loopSelParse(
#  self: var Scone,
#  selTokSeq: seq[TokKind],
#  sepTok: Option[TokKind]=none(TokKind)
#) =
#  var selProcSet: seq[SelParseProc]


proc parseIdent(
  self: var Scone,
  chk: bool,
): bool =
  #echo "parseIdent(): begin: chk:" & $chk
  discard doChkTok(tokIdent)
  let tempStr = self.currTok.optStr.get()
  #echo "parseIdent(): adding this ident: " & tempStr
  self.currIdentStrSeq.add tempStr

proc parseIdentList(
  self: var Scone,
  chk: bool,
): bool =
  discard doChkTok(tokIdent)
  self.loopSelParse(
    selProcSet=(toHashSet([spp(parseIdent)])),
    sepTok=some(tokComma),
  )

proc parseGenericDeclList(
  self: var Scone,
  chk: bool,
): bool =
  result = false

proc subParseGenericDeclList(
  self: var Scone,
  chk: bool,
): bool =
  discard doChkTok(tokLBrace)

proc parseFuncDecl(
  self: var Scone,
  chk: bool,
): bool =
  #echo "parseFuncDecl(): begin: chk:" & $chk
  discard doChkTok(tokDef)
  #echo "parseFuncDecl(): post `tokDef`"
  discard self.parseIdent(chk=false)
  #echo "parseFuncDecl(): " & $self.currIdentStrSeq
  self.lexAndExpect(tokLParen)
  self.lexAndExpect(tokRParen)
  self.lexAndExpect(tokLParen)
  self.lexAndExpect(tokRParen)
  self.lexAndExpect(tokSemicolon)

proc parseStructDecl(
  self: var Scone,
  chk: bool,
): bool =
  discard doChkTok(tokStruct)

proc parseModule*(
  self: var Scone,
  #chk: bool,
) =
  self.lexAndExpect(tokModule)
  self.lexAndExpect(tokIdent)

  # AST stuff here
  #self.mkAst(
  #)
  self.lexAndExpect(tokLParen)

  self.loopSelParse(
    toHashSet([
      spp(parseFuncDecl),
      spp(parseStructDecl),
    ])
  )
    
  
  self.lexAndExpect(tokRParen)
  self.lexAndExpect(tokSemicolon)

