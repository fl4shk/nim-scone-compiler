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

proc selParse(
  self: var Scone,
  selProcSeq: seq[SelParseProc],
): Option[SelParseProc] =
  #for myProc in selProcSeq:
  #  if myProc[](self, true):
  #    return myProc
  for idx in 0 ..< selProcSeq.len():
    self.stackSavedIlp()
    if selProcSeq[idx](self, true):
      self.unstackSavedIlp()
      return some(selProcSeq[idx])
    self.unstackSavedIlp()
    
  return none(SelParseProc)

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
        "expected:" & $tokSet & ", but have " & $self.currTok.tok & " "
      ) & (
        "at this location: " & self.inputFname & ":"
      ) & (
        $self.lineNum & ":" & $self.locInLine
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

proc lexAndCheckOrExpect*(
  self: var Scone,
  chk: bool
): bool =
  if chk:
    discard
  else:
    discard

proc parseFuncDecl(
  self: var Scone,
  chk: bool,
): bool =
  discard
proc parseStructDecl(
  self: var Scone,
  chk: bool,
): bool =
  discard

proc parseModule*(
  self: var Scone,
  chk: bool,
) =
  self.lexAndExpect(tokModule)
  self.lexAndExpect(tokIdent)

  # AST stuff here
  #self.mkAst(
  #)

  self.lexAndExpect(tokLParen)
  discard self.selParse(
    @[
      SelParseProc(parseFuncDecl),
      SelParseProc(parseStructDecl),
    ]
  )
  self.lexAndExpect(tokRParen)
  self.lexAndExpect(tokSemicolon)

