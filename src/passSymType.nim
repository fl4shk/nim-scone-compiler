import std/tables
import std/sets
import std/options

import scone
import dataStructuresMisc
import ast
import symType

type
  SconeSubPassSymType = enum
    #spstHandleImport,
    spstFindTopLevel,
    spstSubstGenerics,
    spstHandleFuncOverloading,
    spstTypeCheck,
    limSpSymType,

  SymTypeArgs = object
    self*: ptr Scone
    changed: bool
    subPass*: SconeSubPassSymType
    ast*: AstNode
    parentSeq*: ptr seq[AstNode]
    moduleIdent*: string

  SymTypeResultKind = enum
    stResultNone,
    stResultIdent,
    stResultU64,
    stResultString,
    stResultBool,
    stResultExpr,
    stResultType,
    stResultPair,
    stResultSeq,

  SymTypeResultPair = object
    left*: SymTypeResult
    right*: SymTypeResult

  SymTypeResult = ref SymTypeResultObj
  SymTypeResultObj = object
    case kind*: SymTypeResultKind
    of stResultNone: myResultNone*: uint8
    of stResultIdent: myIdent*: string
    of stResultString: myString*: string
    of stResultU64: myU64*: uint64
    of stResultBool: myBool*: bool
    of stResultExpr: myExpr*: AstNode
    of stResultType: myType*: AstNode
    of stResultPair: myPair*: SymTypeResultPair
    of stResultSeq: mySeq*: seq[SymTypeResult]

template myDoIt(
  childAst: AstNode,
): untyped =
  args.parentSeq[].add myAst
  var hiddenTempArgs = SymTypeArgs(
    self: args.self,
    subPass: args.subPass,
    ast: childAst,
    #parentSeq: args.parentSeq + @[myAst],
    parentSeq: args.parentSeq,
  )
  #self.doPassSymTypeMain(subPass=subPass, ast=childAst, parent=ast)
  var hiddenTempResult = hiddenTempArgs.doPassSymTypeMain()
  discard args.parentSeq[].pop()
  hiddenTempResult

template myAst(): untyped =
  args.ast
template typeInfo(
  sym: Symbol
): untyped =
  self[].typeInfoS2d[^1][sym.typeInfoIdx]

proc doPassSymTypeMain(
  args: var SymTypeArgs
): SymTypeResult


proc doAstSrcFile(
  args: var SymTypeArgs
): SymTypeResult =
  result = SymTypeResult(
    kind: stResultNone
  )
  let self = args.self
  self[].nextSymTblPass()
  discard myAst.mySrcFile.module.myDoIt()
  for funcDecl in myAst.mySrcFile.funcDeclSeq:
    discard funcDecl.myDoIt()
  for structDecl in myAst.mySrcFile.structDeclSeq:
    discard structDecl.myDoIt()

proc doAstIdent(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    kind: stResultIdent,
    myIdent: myAst.myIdent.strVal,
  )
  let self = args.self
proc doAstU64Lit(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    kind: stResultU64,
    myU64: myAst.myU64Lit.u64Val,
  )
  let self = args.self
proc doAstStrLit(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    kind: stResultString,
    myString: myAst.myStrLit.strLitVal,
  )
  let self = args.self

proc doAstTrue(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    kind: stResultBool,
    myBool: true,
  )
  let self = args.self
proc doAstFalse(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    kind: stResultBool,
    myBool: false,
  )
  let self = args.self

#proc doAstPtr(
#  args: var SymTypeArgs,
#): SymTypeResult =
#  discard
proc doAstDeref(
  args: var SymTypeArgs,
): SymTypeResult =
  discard
proc doAstDot(
  args: var SymTypeArgs,
): SymTypeResult =
  discard
proc doAstVar(
  args: var SymTypeArgs,
): SymTypeResult =
  discard
proc doAstConst(
  args: var SymTypeArgs,
): SymTypeResult =
  discard
proc doAstDef(
  args: var SymTypeArgs,
): SymTypeResult =
  let self = args.self
  let ident = myAst.myDef.ident.myDoIt().myIdent

  case args.subPass:
  of spstFindTopLevel:
    var sym = Symbol(
      moduleName: args.moduleIdent,
      name: ident,
      kind: symFuncDecl,
      initValAstIdx: none(uint64)
    )
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    doAssert(
      false,
      "eek! " & $args.subPass
    )
proc doAstModule(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  #assert(
  #  args.moduleIdent.len() == 0,
  #  "eek! moduleIdent:" & args.moduleIdent & ": " & $myAst
  #)
  args.moduleIdent = myAst.myModule.ident.myDoIt().myIdent
proc doAstStruct(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
  #if args.self.symS2d
proc doAstEnum(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstExtern(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstCextern(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstImport(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstCImport(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstScope(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstIf(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstElif(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstElse(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstSwitch(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstCase(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstDefault(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstFor(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstWhile(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstContinue(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstBreak(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstReturn(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstArray(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstUnop(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstBinop(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstAssignEtc(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstBasicType(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstNamedType(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstType(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstFuncCall(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstStmtExprLhs(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstFuncNamedArgImpl(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstGenericNamedArgImpl(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstGenericList(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  let self = args.self
proc doAstVarEtcDeclMost(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  #let self = args.self

proc doPassSymTypeMain(
  args: var SymTypeArgs
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  case myAst.kind:
  of astSrcFile:
    # `module`: come back to this for when multiple Scone source files are
    # possible to be used in a single program!
    result = args.doAstSrcFile()
  of astIdent:
    result = args.doAstIdent()
  of astU64Lit:
    result = args.doAstU64Lit()
  of astStrLit:
    result = args.doAstStrLit()
  of astTrue:
    result =args.doAstTrue()
  of astFalse:
    result = args.doAstFalse()
  #of astPtr:
  #  result = args.doAstPtr()
  of astDeref:
    result = args.doAstDeref()
  of astDot:
    result = args.doAstDot()
  of astVar:
    result = args.doAstVar()
  of astConst:
    result = args.doAstConst()
  of astDef:
    result = args.doAstDef()
  of astModule:
    result = args.doAstModule()
  of astStruct:
    result = args.doAstStruct()
  of astEnum:
    result = args.doAstEnum()
  of astExtern:
    result = args.doAstExtern()
  of astCextern:
    result = args.doAstCextern()
  of astImport:
    result = args.doAstImport()
  of astCImport:
    result = args.doAstCImport()
  of astScope:
    result = args.doAstScope()
  of astIf:
    result = args.doAstIf()
  of astElif:
    result = args.doAstElif()
  of astElse:
    result = args.doAstElse()
  of astSwitch:
    result = args.doAstSwitch()
  of astCase:
    result = args.doAstCase()
  of astDefault:
    result = args.doAstDefault()
  of astFor:
    result = args.doAstFor()
  of astWhile:
    result = args.doAstWhile()
  of astContinue:
    result = args.doAstContinue()
  of astBreak:
    result = args.doAstBreak()
  of astReturn:
    result = args.doAstReturn()
  of astArray:
    result = args.doAstArray()
  of astUnop:
    result = args.doAstUnop()
  of astBinop:
    result = args.doAstBinop()
  of astAssignEtc:
    result = args.doAstAssignEtc()
  of astBasicType:
    result = args.doAstBasicType()
  of astNamedType:
    result = args.doAstNamedType()
  of astType:
    result = args.doAstType()
  of astFuncCall:
    result = args.doAstFuncCall()
  of astStmtExprLhs:
    result = args.doAstStmtExprLhs()
  of astFuncNamedArgImpl:
    result = args.doAstFuncNamedArgImpl()
  of astGenericNamedArgImpl:
    result = args.doAstGenericNamedArgImpl()
  of astGenericList:
    result = args.doAstGenericList()
  of astVarEtcDeclMost:
    result = args.doAstVarEtcDeclMost()


proc doPassSymType*(
  self: var Scone,
) =
  #var subPass = spstFindTopLevel
  #for subPass in SconeSubPassSymType(0) ..< limSpSymType:

  var parentSeq: seq[AstNode]
  var myArgs = SymTypeArgs(
    self: addr self,
    subPass: spstFindTopLevel,
    ast: self.astRoot,
    parentSeq: addr parentSeq,
  )
  let subPass = addr myArgs.subPass
  while subPass[] < limSpSymType:
    self.nextSymTblPass()
    discard myArgs.doPassSymTypeMain()
    myArgs.parentSeq[].setLen(0)
    var tempSubPass = uint(subPass[])
    tempSubPass += 1
    subPass[] = SconeSubPassSymType(tempSubPass)
    if subPass[] == limSpSymType:
      if myArgs.changed:
        subPass[] = SconeSubPassSymType(1u)
