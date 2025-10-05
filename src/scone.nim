import std/strutils
import std/sequtils
import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import typeInfo
import symTbl

import reduceEtc

#const
#  sconeMaxMacroExpansion*: uint = 1024u

type
  #SconeParserPass* = enum
  #  sconeParserPass0,
  #  sconeParserPassFwdRef,
  SconePass* = enum
    scoPassParse,
    #scoPassSymType,
    scoPassSemanticMacro,
    #scoPassMacroExpansion,
    scoPassEmitC,
    limScoPass,
  SconeSemanticPass* = enum
    scoSemPassCheckDuplNames, # check whether there are any user-defined
                              # types with the same names as user-defined
                              # functions.
                              # This is to be an error!
    scoSemPassHandleExprs,
                              # handle function overloads.
                              # also do type checking.
    limScoSemPass,
  SconeMacroPass* = enum
    scoMacroPassSemFirst,     # First semantic analysis
    scoMacroPassExec,         # execute the macro body (which may call
                              # other functions)
    scoMacroPassReplaceAst,   # Replace the AST of the macro invocation
                              # with the AST returned by the Macro
    scoMacroPassSemSecond,    # Second semantic analysis
    scoMacroPassMaybeRepeat,  # If the AST returned by the macro contains
                              # other macro invocations, th is process
                              # iterates.
    limScoMacroPass,
  #SconeSubPassSymType* = enum
  #  #spstMkSymbolTables,
  #  #spstHandleImport,
  #  spstFirst,
  #  spstRepeat,
  #  #spstSubstGenerics,
  #  #spstHandleFuncOverloading,
  #  #spstTypeCheck,
  #  limSpSymType,

  SconeCurrSymTblInfo* = ref object
    #symTblId*: int
    decls*: SymbolTable
    curr*: SymbolTable
    prev*: SymbolTable
    #astTbl*: OrderedTable[AstNode, int]
    #childIdxSeq*: seq[int]
  Scone* = object
    maxMacroExpansion*: uint #= 1024u
    mode*: Mode
    pass*: SconePass
    semPass*: SconeSemanticPass
    macPass*: SconeMacroPass
    #symTypeSubPass*: SconeSubPassSymType
    inFindAllDecls*: bool
    inFindTypeDecls*: bool
    astRoot*: AstNode

    symTblSeq*: seq[SymbolTable]
    symTblInfo*: SconeCurrSymTblInfo

    savedLexMainSeq*: seq[LexMain]

    lexMain*: LexMain
    parentTempSeq*: seq[AstNode]
    
    inputFname*: string
    inp*: string
    outp*: string

proc locMsg*(
  self: var Scone
): string

proc nextSymTblPass*(
  self: var Scone,
) =
  var toAdd = SymbolTable()
  self.symTblSeq.add toAdd
  let info = addr self.symTblInfo

  if info[].curr != nil and info[].prev == nil:
    echo "nextSymTblPass(): "
    echo info[].curr.toStr()
    info[].decls = info.curr

  info[].prev = info[].curr
  info[].curr = self.symTblSeq[^1]

  #if self.symTblSeq.len() == 3:
  #  self.symTblSeq.delete(0 .. 0)

  if self.symTblSeq.len() == 4:
    self.symTblSeq.delete(1 .. 1)


proc addChildSymTbl(
  self: var Scone,
  #child: var SymbolTable,
  ast: AstNode,
) =
  #--------
  # BEGIN: old version
  #self.currSymTbl.childSeq.add child
  #self.currSymTbl.childSeq[^1].parent = self.currSymTbl
  #self.currSymTbl = self.currSymTbl.childSeq[^1]
  # END: old version
  #--------
  let info = addr self.symTblInfo
  var child = SymbolTable(
    ast: ast
  )

  if info[].prev != nil:
    info[].prev = info[].prev.childSeq[info[].curr.childSeq.len()]

  info[].curr.childSeq.add child
  info[].curr.childSeq[^1].parent = info[].curr
  info[].curr = info[].curr.childSeq[^1]

#proc mkSymbolTableMain(
#  self: var Scone,
#  ast: AstNode,
#): SymbolTable =
#  #var childSymTbl = SymbolTable(
#  #  ast: ast
#  #)
#  self.addChildSymTbl(
#    #child=childSymTbl
#    ast=ast
#  )
#  result = self.symTblInfo.curr

proc gotoParentSymTbl*(
  self: var Scone,
) =
  #self.currSymTbl = self.currSymTbl.parent
  let info = addr self.symTblInfo
  #echo sconcat(@[
  #  "gotoParentSymTbl: parent != nil: ",
  #  $(info[].curr.parent != nil)
  #])
  #if info[].curr.childSeq.len() == 1:
  if info[].curr.parent == nil:
    doAssert(
      false,
      sconcat(@[
        "gotoParentSymTbl: debug msg: ",
        $(info[].curr.parent != nil)
      ])
    )
  info[].curr = info[].curr.parent
  if info[].prev != nil:
    info[].prev = info[].prev.parent



#proc sameFuncDeclSignature*(
#  self: var Scone,
#  leftSym: Symbol,
#  rightSym: Symbol,
#): bool =
#  #result = false
#  #let cmpNe = (leftSym != rightSym)
#
#  let cmpName = (leftSym.name == rightSym.name)
#  #echo "sameFuncDeclSignature: " & $cmpNe & " " & $cmpName
#  echo "sameFuncDeclSignature: " & $cmpName
#  echo "  " & leftSym.name & " " & rightSym.name
#  result = (
#    (
#      #cmpNe
#      true
#    ) and (
#      cmpName
#    )
#  )

proc sameLexMainEtc*(
  self: var Scone,
  left: SymbolTable,
  right: SymbolTable
): bool =
  let cmpNe = (
    left != right
  )
  let cmpSymIsSome = (
    left.sym.isSome and right.sym.isSome
  )
  var cmpName: bool = false
  var subCmpInputFname: bool = false
  if cmpSymIsSome:
    cmpName = (
      left.sym.get().name == right.sym.get().name
    )
    subCmpInputFname = (
      left.sym.get().inputFname == right.sym.get().inputFname
    )
  doAssert(
    left.ast != nil,
    "eek!"
  )
  doAssert(
    right.ast != nil,
    "eek!"
  )
  result = (
    andR(@[
      subCmpInputFname,
      left.ast.lexMain.locInLine == right.ast.lexMain.locInLine,
      left.ast.lexMain.lineNum == right.ast.lexMain.lineNum,
    ])
  )

#proc sameType*(
#  self: var Scone,
#  left: SymbolTable,
#  right: SymbolTable,
#): bool =
#  result = false
#  if not andR(@[left.sym.isSome, right.sym.isSome]):
#    return
#
#  let lSym = left.sym.get()
#  let rSym = right.sym.get()
#  let lTinfo = lSym.typeInfo
#  let rTinfo = rSym.typeInfo
#
#  if lTinfo.kind != rTinfo.kind:
#    return
#
#  case lTinfo.kind:
#  of tiToResolve:
#    return
#  of tiBasicType:
#    return (
#      lTinfo.myBasicType.kind == rTinfo.myBasicType.kind
#    )
#  of tiStruct:
#    discard
#  of tiFunc:
#    discard

#type
#  ScoreTree = ref ScoreTreeObj
#
#  ScoreTreeObj = object
#    #scone: ptr Scone
#    ast: AstNode
#    valid: bool
#    parent: ScoreTree
#    childSeq: seq[ScoreTree]
#
#proc mkScoreTree(
#  self: var Scone,
#  ast: AstNode,
#  #ast: AstNode=nil,
#  parent: ScoreTree=nil,
#): ScoreTree =
#  doAssert(
#    ast != nil,
#    sconcat(@[
#      "eek!"
#    ])
#  )
#  result = ScoreTree(
#    ast: ast,
#    valid: false,
#    parent: parent,
#  )
#  #if ast == nil:
#  #  doAssert(
#  #    parent == nil,
#  #    sconcat(@[
#  #      "eek! ", 
#  #      "funcCallAst:", $funcCallAst,
#  #      "parent:", $parent[]
#  #    ])
#  #  )
#  #  result = ScoreTree(
#  #    ast: funcCallAst,
#  #    parent: nil,
#  #  )
#  #else:
#  #  doAssert(
#  #    ast != nil,
#  #    sconcat(@[
#  #      "eek! ", 
#  #      "ast:", $ast,
#  #      "parent:", $parent[]
#  #    ])
#  #  )
#  #  result = ScoreTree(
#  #    ast: ast,
#  #    parent: parent,
#  #  )


#type
#  TypeTree = ref TypeTreeObj
#  TypeTreeObj = object
#    ast*: AstNode
#    parent*: TypeTree
#    childSeq*: 


#proc lookupAndSetSymTblId*(
#  self: var Scone,
#  ast: AstNode,
#): Option[SymbolTable] =
#  result = none(SymbolTable)
#  case ast.kind:
#  of astIdent:
#    discard
#  of astU64Lit:
#    discard
#  of astStrLit:
#    discard
#  #of astOpenarrLit:
#  #  discard
#  of astTrue:
#    discard
#  of astFalse:
#    discard
#  of astDeref:
#    discard
#  of astDot:
#    discard
#  #of astVar:
#  #  discard
#  #of astConst:
#  #  discard
#  of astUnop:
#    discard
#  of astBinop:
#    discard
#  of astFuncCall:
#    discard
#  of astFuncNamedArgImpl:
#    discard
#  of astGenericNamedArgImpl:
#    discard
#  of astVarEtcDeclMost:
#    discard
#  else:
#    doAssert(
#      false,
#      sconcat(@[
#        "eek! ", $ast
#      ])
#    )

proc isBetterMatch(
  self: var Scone,
  funcCall: AstNode,
  funcDecl0: SymbolTable,
  funcDecl1: SymbolTable,
): bool =
  result = false

  doAssert(
    funcDecl0.sym.isSome,
    sconcat(@[
      "eek! ", $funcDecl0.ast
    ])
  )
  doAssert(
    funcDecl1.sym.isSome,
    sconcat(@[
      "eek! ", $funcDecl1.ast
    ])
  )
  let sym0 = funcDecl0.sym.get()
  let sym1 = funcDecl1.sym.get()
  let tinfo0 = sym0.typeInfo
  let tinfo1 = sym1.typeInfo

  if orR(@[
    sym0.kind != symFuncDecl,
    sym1.kind != symFuncDecl,
    tinfo0.kind != tiFunc,
    tinfo1.kind != tiFunc,
    tinfo0.myFunc.argIdxSeq.len() != tinfo1.myFunc.argIdxSeq.len(),
  ]):
    return

  #proc cmpAst(
  #  self: var Scone,
  #  callArg: AstNode,
  #  declArg: AstNode,
  #  idx: int,
  #): int32 =
  #  result = 0

  #proc myMatchFunc(
  #  self: var Scone,
  #  param: AstNode,
  #  ast0: AstNode,
  #  ast1: AstNode,
  #): int32 =
  #  result = 0
  #  #var cmp0 = self.cmpAst(param=param, ast=ast0)
  #  #var cmp1 = self.cmpAst(param=param, ast=ast1)
  #  #if cmp0 < cmp1:
  #  #  discard
  #  #elif cmp0 > cmp1:
  #  #  discard
  #  #else:
  #  #  discard
  #var mySeq0: seq[int32] = @[]
  #var mySeq1: seq[int32] = @[]
  #for idx in 0 ..< tinfo0.myFunc.argIdxSeq.len():
  #  discard

  #var isGeneric0: bool = false
  #var isGeneric1: bool = false

proc gatherFuncDecls*(
  self: var Scone,
  #name: string
  funcCallExpr: AstExpr,
): seq[SymbolTable] =
  doAssert(
    funcCallExpr.kind == exprFuncCall,
    sconcat(@[
      "eek! ", $funcCallExpr[],
    ])
  )
  doAssert(
    self.symTblInfo.decls != nil,
    "eek!"
  )
  let info = addr self.symTblInfo
  let decls = info[].decls
  let name = funcCallExpr.myFuncCall.ident.strVal
  doAssert(
    name in decls.nameTbl,
    sconcat(@[
      "Unknown called function of name ",
      "\"", name, "\" ",
      funcCallExpr.lexMain.locMsg(self.inputFname)
    ])
  )
  #var scoreTree = ScoreTree(
  #)

  #var highestScore: int = 0

  let myIdxSeq = decls.nameTbl[name]
  for idx in myIdxSeq:
    let child = decls.childSeq[idx]
    result.add child

proc resolveFuncCallOverload*(
  self: var Scone,
  funcCallExpr: AstExpr,
): Option[SymbolTable] =
  result = none(SymbolTable)
  let mySeq = self.gatherFuncDecls(funcCallExpr)
  let name = funcCallExpr.myFuncCall.ident.strVal
  doAssert(
    mySeq.len() == 1,
    sconcat(@[
      "Unable to automatically resolve overload of function of name ",
      "\"", name, "\"",
      "(", funcCallExpr.lexMain.locMsg(inputFname=self.inputFname), ")"
    ]),
  )
  result = some(mySeq[0])
  #doAssert(
  #  funcCall.sym.isSome,
  #  sconcat(@[
  #    "eek! ", $funcCall[],
  #  ])
  #)
  #let prev = addr sel


#proc sameFuncDeclSignature*(
#  self: var Scone,
#  left: SymbolTable,
#  right: SymbolTable,
#): bool =
#  let cmpLexMainEtc = (
#    not self.sameLexMainEtc(
#      left=left,
#      right=right,
#    )
#  )
#  let cmpType = (
#    self.sameType(
#      left=left,
#      right=right,
#    )
#  )
#  # TODO: support checking the types of the arguments
#  result = (
#    #cmpNe and cmpSymIsSome and cmpName and cmpLexMainEtc
#    cmpLexMainEtc and cmpType
#  )
#
#proc findDuplFuncMain*(
#  self: var Scone,
#  parent: var SymbolTable,
#  toChk: var SymbolTable,
#  #sym: Symbol,
#): Option[SymbolTable] =
#  result = none(SymbolTable)
#
#  let optSym = toChk.sym
#  if optSym.isSome:
#    let sym = optSym.get()
#    case sym.kind:
#    of symFuncDecl:
#      if sym.name in parent.nameTbl:
#        let myIdxSeq = parent.nameTbl[sym.name] 
#        for idx in myIdxSeq:
#          let child = parent.childSeq[idx]
#          doAssert(
#            child.sym.isSome,
#            $child[]
#          )
#          if self.sameFuncDeclSignature(child, toChk):
#            return some(child)
#    else:
#      discard
#    #--------
#  #if not result.isSome and parent.parent != nil:
#  if parent.parent != nil:
#    result = self.findDuplFuncMain(
#      parent=parent.parent,
#      toChk=toChk,
#    )
#  
#proc findDuplFunc*(
#  self: var Scone,
#  #sym: Symbol
#): Option[SymbolTable] =
#  let info = addr self.symTblInfo
#  result = self.findDuplFuncMain(
#    #toChk=self.symTblInfo.curr,
#    #sym=sym
#    parent=info[].curr.parent,
#    toChk=info[].curr,
#  )

proc checkDuplSym*(
  self: var Scone,
  #sym: Symbol,
) =
  let info = addr self.symTblInfo
  let optSym = info[].curr.sym
  if optSym.isSome:
    let sym = optSym.get()
    case sym.kind:
    of symFuncDecl:
      #let dupFunc = self.findDuplFunc()
      #doAssert(
      #  not dupFunc.isSome,
      #  sconcat(@[
      #    "Error: duplicate function signature for function of name ",
      #    "\"", sym.name, "\" ",
      #    "(current instance ",
      #      info[].curr.ast.lexMain.locMsg(inputFname=self.inputFname),
      #    ") ",
      #    "(previous instance ",
      #      dupFunc.get().ast.lexMain.locMsg(
      #        inputFname=dupFunc.get().sym.get().inputFname
      #      ),
      #    ")"
      #  ])
      #)
      # wait until we attempt to resolve function overloads
      discard
    else:
      let mySymTbl = info[].curr.parent
      doAssert(
        (
          orR(@[
            sym.name notin mySymTbl.nameTbl,
            mySymTbl.nameTbl[sym.name].len() <= 1,
          ])
        ),
        sconcat(@[
          "Error: duplicate non-function symbol of name ",
          "\"", sym.name, "\" ",
          "(current instance ",
            (
              mySymTbl.childSeq[mySymTbl.nameTbl[sym.name][1]].ast
            ).lexMain.locMsg(inputFname=self.inputFname),
          ") ",
          "(previous instance ",
            (
              mySymTbl.childSeq[mySymTbl.nameTbl[sym.name][0]].ast
            ).lexMain.locMsg(inputFname=self.inputFname),
          ")"
        ])
      )

proc addSym*(
  self: var Scone,
  sym: Option[Symbol],
  ast: AstNode,
  #typeInfo: TypeInfo,
) =
  let info = addr self.symTblInfo
  self.addChildSymTbl(ast=ast)
  if sym.isSome:
    let parent = info[].curr.parent
    if sym.get().name notin parent.nameTbl:
      parent.nameTbl[sym.get().name] = @[parent.childSeq.len() - 1]
    else:
      parent.nameTbl[sym.get().name].add parent.childSeq.len() - 1
    info[].curr.sym = sym

proc locInLine*(
  self: var Scone
): var uint64 =
  return self.lexMain.locInLine
proc lineNum*(
  self: var Scone
): var uint64 =
  return self.lexMain.lineNum
proc inpIdx*(
  self: var Scone
): var int =
  return self.lexMain.inpIdx
proc currTok*(
  self: var Scone
): var CurrTok =
  return self.lexMain.currTok

#proc stackIdentStrSeq*(
#  self: var Scone,
#) =
#  #self.identStrS2d
#  discard


proc stackSavedIlp*(
  self: var Scone
) =
  self.savedLexMainSeq.add self.lexMain

proc unstackSavedIlp*(
  self: var Scone
) =
  #let oldLenMinus1 = self.savedLexMainSeq.len() - 1

  #self.lexMain = self.savedLexMainSeq[oldLenMinus1]
  #self.savedLexMainSeq.setLen(oldLenMinus1)
  self.lexMain = self.savedLexMainSeq.pop()

proc locMsg*(
  self: var Scone
): string =
  result = self.lexMain.locMsg(inputFname=self.inputFname)
