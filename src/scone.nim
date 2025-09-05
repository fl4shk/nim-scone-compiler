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
    #scoPassEmitC,
    limScoPass,

  #SconeModule* = object
  #  name*: string
  #  symS2dIdx*: uint64

  SconeCurrSymTblInfo* = object
    curr*: SymbolTable
    prev*: SymbolTable
    #childIdxSeq*: seq[int]
  Scone* = object
    mode*: Mode
    pass*: SconePass
    inFindDecls*: bool
    #macroLim*: uint
    #parserPass*: SconeParserPass
    #ast*: seq[AstNode]
    astRoot*: AstNode
    #ast*: AstNode
    #currAstIdx*: uint64

    #symS2d*: seq[seq[Symbol]]
    #typeInfoS2d*: seq[seq[TypeInfo]]
    ##symNameToIdxTblS2d*: seq[seq[OrderedTable[string, seq[uint64]]]]
    #symIdxTblSeq*: seq[OrderedTable[string, seq[uint64]]]
    #moduleTblSeq*: seq[OrderedTable[string, SconeModule]]
    symTblSeq*: seq[SymbolTable]
    #prevCurrSymTbl*: SymbolTable
    mySymTblInfo*: SconeCurrSymTblInfo

    savedLexMainSeq*: seq[LexMain]

    lexMain*: LexMain
    #parentExprS2d*: seq[ptr AstNode]
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
  #let cmpLexMain = left.scopeAst.lexMain.inputFname
  #echo "left[]:" & $left[]
  #echo "right[]:" & $right[]
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
  #echo (
  #  (
  #    "testificate: "
  #  ) & (
  #    $cmpNe & " " & $cmpSymIsSome & " " & $cmpName & " " & $cmpLexMainEtc
  #  )
  #)
  #if left.sym.isSome:
  #  echo (
  #    "left.sym.isSome:" & left.sym.get().name
  #  )
  #if right.sym.isSome:
  #  echo (
  #    "right.sym.isSome:" & right.sym.get().name
  #  )

proc findDuplFuncMain*(
  self: var Scone,
  parent: var SymbolTable,
  toChk: var SymbolTable,
  #sym: Symbol,
): Option[SymbolTable] =
  result = none(SymbolTable)
  #echo "here's a test: " & sym.name & " " & $sym.kind

  let optSym = toChk.sym
  if optSym.isSome:
    let sym = optSym.get()
    case sym.kind:
    of symFuncDecl:
      #echo (
      #  "sym.name, in tbl? " & (sym.name) & " " & $(sym.name in parent.tbl)
      #)
      if sym.name in parent.tbl:
        let myIdxSeq = parent.tbl[sym.name] 
        for idx in myIdxSeq:
          let child = parent.childSeq[idx]
          doAssert(
            child.sym.isSome,
            $child[]
          )
          #if child.sym.isSome:
          #  #if self.sameFuncSignature(sym, child.sym.get()):
          #  #  result = some(child)
          if self.sameFuncSignature(child, toChk):
            return some(child)
      #if not result.isSome and parent.parent != nil:
      #  result = self.findDuplFuncMain(
      #    #parent=parent.parent, sym=sym
      #    #parent=parent.parent, toChk=parent
      #    parent=parent.parent,
      #    toChk=toChk,
      #  )
    else:
      #echo "what (sym.kind)? " & $sym.kind
      discard
    #--------
    if not result.isSome and parent.parent != nil:
      result = self.findDuplFuncMain(
        #parent=parent.parent, sym=sym
        #parent=parent.parent, toChk=parent
        parent=parent.parent,
        toChk=toChk,
      )
  #else:
  #  echo "what (not optSym.isSome)? " #& $sym.kind
  
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
  #let currTbl = info[].curr
  #currTbl.childSeq.add SymbolTable()
  #currTbl.childS
  self.addChildSymTbl(scopeAst=scopeAst)
  if sym.isSome:
    #echo "addSym: sym.isSome: " & sym.get().name
    let parent = info[].curr.parent
    if sym.get().name notin parent.tbl:
      #var toAdd: seq[int] = @[currTbl.childSeq.len()]
      parent.tbl[sym.get().name] = @[parent.childSeq.len() - 1]
    else:
      parent.tbl[sym.get().name].add parent.childSeq.len() - 1
    #info[].curr.childSeq[^1].sym = sym
    #info[].curr.childSeq.add sym
    info[].curr.sym = sym

  #currTbl.symSeq.add sym
  #currTbl.symSeq[^1].typeInfoIdx = uint32(currTbl.typeInfoSeq.len())
  #currTbl.typeInfoSeq.add typeInfo

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
  #echo "stackSavedIlp(): " & $self.currTok
  self.savedLexMainSeq.add self.lexMain
  #self.savedLocInLineSeq.add self.locInLine
  #self.savedLineNumSeq.add self.lineNum
  #self.savedInpIdxSeq.add self.inpIdx
  #self.savedCurrTokSeq.add self.currTok

proc unstackSavedIlp*(
  self: var Scone
) =
  #let oldLenMinus1 = self.savedLocInLineSeq.len() - 1
  let oldLenMinus1 = self.savedLexMainSeq.len() - 1
  #echo "unstackSavedIlp(): before: " & $self.currTok

  self.lexMain = self.savedLexMainSeq[oldLenMinus1]
  #echo "unstackSavedIlp(): after: " & $self.currTok
  self.savedLexMainSeq.setLen(oldLenMinus1)

  #self.locInLine = self.savedLocInLineSeq[oldLenMinus1]
  #self.lineNum = self.savedLineNumSeq[oldLenMinus1]
  #self.inpIdx = self.savedInpIdxSeq[oldLenMinus1]
  #self.currTok = self.savedCurrTokSeq[oldLenMinus1]
  ##echo "unstackSavedIlp(): after: " & $self.currTok

  #self.savedLocInLineSeq.setLen(oldLenMinus1)
  #self.savedLineNumSeq.setLen(oldLenMinus1)
  #self.savedInpIdxSeq.setLen(oldLenMinus1)
  #self.savedCurrTokSeq.setLen(oldLenMinus1)

proc locMsg*(
  self: var Scone
): string =
  result = self.lexMain.locMsg(inputFname=self.inputFname)
