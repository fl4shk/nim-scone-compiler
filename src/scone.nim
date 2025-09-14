import std/strutils
import std/sequtils
import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import symType

import reduceEtc

#const
#  sconeMaxMacroExpansion*: uint = 1024u

type
  #SconeParserPass* = enum
  #  sconeParserPass0,
  #  sconeParserPassFwdRef,
  SconePass* = enum
    scoPassParse,
    scoPassSymType,
    #scoPassMacroExpansion,
    scoPassEmitC,
    limScoPass,
  SconeSubPassSymType* = enum
    #spstMkSymbolTables,
    #spstHandleImport,
    spstFindTopLevelDecls,
    spstSubstGenerics,
    spstHandleFuncOverloading,
    spstTypeCheck,
    limSpSymType,

  SconeCurrSymTblInfo* = object
    decls*: SymbolTable
    curr*: SymbolTable
    prev*: SymbolTable
    astTbl*: OrderedTable[AstNode, int]
    #childIdxSeq*: seq[int]
  Scone* = object
    maxMacroExpansion*: uint #= 1024u
    mode*: Mode
    pass*: SconePass
    symTypeSubPass*: SconeSubPassSymType
    inFindAllDecls*: bool
    inFindTypeDecls*: bool
    astRoot*: AstNode

    symTblSeq*: seq[SymbolTable]
    mySymTblInfo*: SconeCurrSymTblInfo

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
  let info = addr self.mySymTblInfo

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
  let info = addr self.mySymTblInfo
  var child = SymbolTable(
    ast: ast
  )

  if info[].prev != nil:
    info[].prev = info[].prev.childSeq[info[].curr.childSeq.len()]

  #echo sconcat(@[
  #  "addChildSymTbl: childSeq.len(): ", $info[].curr.childSeq.len()
  #])
  #echo sconcat(@[
  #  "addChildSymTbl: child: ", $child[]
  #])
  #if info[].curr.childSeq.len() == 1:
  #  doAssert(
  #    false,
  #    sconcat(@[
  #      "addChildSymTbl: debug msg: ",
  #      $(info[].curr.parent != nil),
  #    ])
  #  )
  info[].curr.childSeq.add child
  info[].curr.childSeq[^1].parent = info[].curr
  #echo sconcat(@[
  #  "addChildSymTbl: post set parent: ",
  #  $(info[].curr.childSeq[^1].parent != nil), " ",
  #  $(info[].curr.childSeq.len())
  #])
  info[].curr = info[].curr.childSeq[^1]
  #self.mySymTblInfo.childIdxSeq

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
#  result = self.mySymTblInfo.curr

proc gotoParentSymTbl*(
  self: var Scone,
) =
  #self.currSymTbl = self.currSymTbl.parent
  let info = addr self.mySymTblInfo
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

proc sameType*(
  self: var Scone,
  left: SymbolTable,
  right: SymbolTable,
): bool =
  result = false
  if not andR(@[left.sym.isSome, right.sym.isSome]):
    return

  let lSym = left.sym.get()
  let rSym = right.sym.get()
  let lTinfo = lSym.typeInfo
  let rTinfo = rSym.typeInfo

  if lTinfo.kind != rTinfo.kind:
    return

  case lTinfo.kind:
  of tiToResolve:
    return
  of tiBasicType:
    return (
      lTinfo.myBasicType.kind == rTinfo.myBasicType.kind
    )
  of tiStruct:
    discard
  of tiFunc:
    discard

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
  funcCallAst: AstNode,
): seq[SymbolTable] =
  doAssert(
    funcCallAst.kind == astFuncCall,
    sconcat(@[
      "eek! ", $funcCallAst[],
    ])
  )
  doAssert(
    self.mySymTblInfo.decls != nil,
    "eek!"
  )
  let info = addr self.mySymTblInfo
  let decls = info[].decls
  let name = funcCallAst.myFuncCall.ident.myIdent.strVal
  doAssert(
    name in decls.tbl,
    sconcat(@[
      "Unknown called function of name ",
      "\"", name, "\" ",
      funcCallAst.lexMain.locMsg(self.inputFname)
    ])
  )
  #var scoreTree = ScoreTree(
  #)

  #var highestScore: int = 0

  let myIdxSeq = decls.tbl[name]
  for idx in myIdxSeq:
    let child = decls.childSeq[idx]
    result.add child

proc resolveFuncCallOverload*(
  self: var Scone,
  funcCallAst: AstNode,
): Option[SymbolTable] =
  result = none(SymbolTable)
  let mySeq = self.gatherFuncDecls(funcCallAst)
  let name = funcCallAst.myFuncCall.ident.myIdent.strVal
  doAssert(
    mySeq.len() == 1,
    sconcat(@[
      "Unable to automatically resolve overload of function of name ",
      "\"", name, "\"",
      "(", funcCallAst.lexMain.locMsg(inputFname=self.inputFname), ")"
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
#      if sym.name in parent.tbl:
#        let myIdxSeq = parent.tbl[sym.name] 
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
#  let info = addr self.mySymTblInfo
#  result = self.findDuplFuncMain(
#    #toChk=self.mySymTblInfo.curr,
#    #sym=sym
#    parent=info[].curr.parent,
#    toChk=info[].curr,
#  )

proc checkDuplSym*(
  self: var Scone,
  #sym: Symbol,
) =
  let info = addr self.mySymTblInfo
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
            sym.name notin mySymTbl.tbl,
            mySymTbl.tbl[sym.name].len() <= 1,
          ])
        ),
        sconcat(@[
          "Error: duplicate non-function symbol of name ",
          "\"", sym.name, "\" ",
          "(current instance ",
            (
              mySymTbl.childSeq[mySymTbl.tbl[sym.name][1]].ast
            ).lexMain.locMsg(inputFname=self.inputFname),
          ") ",
          "(previous instance ",
            (
              mySymTbl.childSeq[mySymTbl.tbl[sym.name][0]].ast
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
  let info = addr self.mySymTblInfo
  self.addChildSymTbl(ast=ast)
  if sym.isSome:
    let parent = info[].curr.parent
    #echo sconcat(@[
    #  "addSym: parent: ", $parent[]
    #])
    #echo sconcat(@[
    #  "addSym: curr: ", $info[].curr[], " ", $info[].curr.childSeq.len()
    #])
    #echo sconcat(@[
    #  "addSym: sym: ", $sym.get()[]
    #])
    #echo sconcat(@[
    #  "addSym: tinfo: ", $sym.get().typeInfo[]
    #])
    if sym.get().name notin parent.tbl:
      #echo sconcat(@[
      #  "addsym: testificate: ", $(parent.childSeq.len() - 1)
      #])
      parent.tbl[sym.get().name] = @[parent.childSeq.len() - 1]
    else:
      parent.tbl[sym.get().name].add parent.childSeq.len() - 1
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
