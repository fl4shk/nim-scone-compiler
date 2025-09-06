import std/strutils
import std/sequtils
import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import symType

const
  sconeMaxMacroExpansion*: uint = 1024u

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

  SconeCurrSymTblInfo* = object
    curr*: SymbolTable
    prev*: SymbolTable
    #childIdxSeq*: seq[int]
  Scone* = object
    mode*: Mode
    pass*: SconePass
    inFindDecls*: bool
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
  #if self.symS2d.len() == 0:
  var toAdd = SymbolTable()
  self.symTblSeq.add toAdd
  #self.currSymTbl[1] = self.symTblSeq[^1]
  #self.currSymTbl
  let info = addr self.mySymTblInfo

  if info[].curr != nil and info[].prev == nil:
    echo "nextSymTblPass(): "
    echo info[].curr.toStr()

  #if info[].curr != nil:
  info[].prev = info[].curr
  info[].curr = self.symTblSeq[^1]

  if self.symTblSeq.len() == 3:
    self.symTblSeq.delete(0 .. 0)
  #if self.symTblSeq.len() == 2:
  #  info[].prev = self.symTblSeq[^2]


proc addChildSymTbl(
  self: var Scone,
  #child: var SymbolTable,
  scopeAst: AstNode,
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
    scopeAst: scopeAst
  )

  if info[].prev != nil:
    #echo $info[].prev.scopeAst
    #echo "curr.childSeq.len():" & $info[].curr.childSeq.len()
    #echo "prev.childSeq.len():" & $info[].prev.childSeq.len()
    #echo "curr.parent == nil: " & $(info[].curr.parent == nil)
    #echo "prev.parent == nil: " & $(info[].prev.parent == nil)
    info[].prev = info[].prev.childSeq[info[].curr.childSeq.len()]

  info[].curr.childSeq.add child
  info[].curr.childSeq[^1].parent = info[].curr
  info[].curr = info[].curr.childSeq[^1]
  #self.mySymTblInfo.childIdxSeq

proc mkSymbolTableMain(
  self: var Scone,
  scopeAst: AstNode,
): SymbolTable =
  #var childSymTbl = SymbolTable(
  #  scopeAst: scopeAst
  #)
  self.addChildSymTbl(
    #child=childSymTbl
    scopeAst=scopeAst
  )
  result = self.mySymTblInfo.curr

proc gotoParentSymTbl*(
  self: var Scone,
) =
  #self.currSymTbl = self.currSymTbl.parent
  let info = addr self.mySymTblInfo
  info[].curr = info[].curr.parent
  if info[].prev != nil:
    info[].prev = info[].prev.parent

#proc sameFuncSignature*(
#  self: var Scone,
#  leftSym: Symbol,
#  rightSym: Symbol,
#): bool =
#  #result = false
#  #let cmpNe = (leftSym != rightSym)
#
#  let cmpName = (leftSym.name == rightSym.name)
#  #echo "sameFuncSignature: " & $cmpNe & " " & $cmpName
#  echo "sameFuncSignature: " & $cmpName
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
    left.scopeAst != nil,
    "eek!"
  )
  doAssert(
    right.scopeAst != nil,
    "eek!"
  )
  result = (
    (
      subCmpInputFname
    ) and (
      left.scopeAst.lexMain.locInLine == right.scopeAst.lexMain.locInLine
    ) and (
      left.scopeAst.lexMain.lineNum == right.scopeAst.lexMain.lineNum
    )
  )
proc sameType*(
  self: var Scone,
  left: SymbolTable,
  right: SymbolTable,
): bool =
  result = false

proc sameFuncSignature*(
  self: var Scone,
  left: SymbolTable,
  right: SymbolTable,
): bool =
  let cmpLexMainEtc = (
    not self.sameLexMainEtc(
      left=left,
      right=right,
    )
  )
  let cmpType = (
    self.sameType(
      left=left,
      right=right,
    )
  )
  # TODO: support checking the types of the arguments
  result = (
    #cmpNe and cmpSymIsSome and cmpName and cmpLexMainEtc
    cmpLexMainEtc and cmpType
  )

proc findDuplFuncMain*(
  self: var Scone,
  parent: var SymbolTable,
  toChk: var SymbolTable,
  #sym: Symbol,
): Option[SymbolTable] =
  result = none(SymbolTable)

  let optSym = toChk.sym
  if optSym.isSome:
    let sym = optSym.get()
    case sym.kind:
    of symFuncDecl:
      if sym.name in parent.tbl:
        let myIdxSeq = parent.tbl[sym.name] 
        for idx in myIdxSeq:
          let child = parent.childSeq[idx]
          doAssert(
            child.sym.isSome,
            $child[]
          )
          if self.sameFuncSignature(child, toChk):
            return some(child)
    else:
      discard
    #--------
  #if not result.isSome and parent.parent != nil:
  if parent.parent != nil:
    result = self.findDuplFuncMain(
      parent=parent.parent,
      toChk=toChk,
    )
  
proc findDuplFunc*(
  self: var Scone,
  #sym: Symbol
): Option[SymbolTable] =
  let info = addr self.mySymTblInfo
  result = self.findDuplFuncMain(
    #toChk=self.mySymTblInfo.curr,
    #sym=sym
    parent=info[].curr.parent,
    toChk=info[].curr,
  )

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
      let dupFunc = self.findDuplFunc()
      doAssert(
        not dupFunc.isSome,
        (
          "Error: duplicate function signature for function of name \""
        ) & (
          sym.name & "\" "
        ) & (
          "(current instance "
        ) & (
          info[].curr.scopeAst.lexMain.locMsg(inputFname=self.inputFname)
        ) & (
          ") (previous instance "
        ) & (
          dupFunc.get().scopeAst.lexMain.locMsg(
            inputFname=dupFunc.get().sym.get().inputFname
          )
        ) & (
          ")"
        )
      )
    else:
      let mySymTbl = info[].curr.parent
      doAssert(
        (
          (
            sym.name notin mySymTbl.tbl
          ) or (
            mySymTbl.tbl[sym.name].len() <= 1
          )
        ),
        (
          "Error: duplicate non-function symbol of name \""
        ) & (
          sym.name & "\" "
        ) & (
          "(current instance "
        ) & (
          (
            mySymTbl.childSeq[mySymTbl.tbl[sym.name][1]].scopeAst
          ).lexMain.locMsg(inputFname=self.inputFname)
        ) & (
          ") (previous instance "
        ) & (
          (
            mySymTbl.childSeq[mySymTbl.tbl[sym.name][0]].scopeAst
          ).lexMain.locMsg(inputFname=self.inputFname)
        ) & (
          ")"
        )
      )

proc addSym*(
  self: var Scone,
  sym: Option[Symbol],
  scopeAst: AstNode,
  #typeInfo: TypeInfo,
) =
  let info = addr self.mySymTblInfo
  self.addChildSymTbl(scopeAst=scopeAst)
  if sym.isSome:
    let parent = info[].curr.parent
    if sym.get().name notin parent.tbl:
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
