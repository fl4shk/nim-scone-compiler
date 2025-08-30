import std/tables
import std/sets
import std/options

import scone
import dataStructuresMisc
import ast
import symType

type
  SconeSubPassSymType = enum
    #spSymTypeHandleImport,
    spSymTypeFindTopLevel,
    spSymTypeSubstGenerics,
    limSpSymType,

  SymTypeArgs = object
    self*: ptr Scone
    subPass*: SconeSubPassSymType
    ast*: AstNode
    parentSeq*: seq[AstNode]

template myDoIt(
  childAst: AstNode,
) =
  var hiddenTempArgs = SymTypeArgs(
    self: addr self,
    subPass: subPass,
    ast: childAst,
    parentSeq: parentSeq + @[ast],
  )
  #self.doPassSymTypeMain(subPass=subPass, ast=childAst, parent=ast)
  hiddenTempArgs.doPassSymTypeMain()

proc doPassSymTypeMain(
  args: var SymTypeArgs
)


proc doAstSrcFile(
  args: var SymTypeArgs
) =
  discard

proc doAstIdent(
  args: var SymTypeArgs,
) =
  discard
proc doAstU64Lit(
  args: var SymTypeArgs,
) =
  discard
proc doAstStrLit(
  args: var SymTypeArgs,
) =
  discard
proc doAstTrue(
  args: var SymTypeArgs,
) =
  discard
proc doAstFalse(
  args: var SymTypeArgs,
) =
  discard
proc doAstPtr(
  args: var SymTypeArgs,
) =
  discard
proc doAstDeref(
  args: var SymTypeArgs,
) =
  discard
proc doAstDot(
  args: var SymTypeArgs,
) =
  discard
proc doAstVar(
  args: var SymTypeArgs,
) =
  discard
proc doAstConst(
  args: var SymTypeArgs,
) =
  discard
proc doAstDef(
  args: var SymTypeArgs,
) =
  discard
proc doAstModule(
  args: var SymTypeArgs,
) =
  discard
proc doAstStruct(
  args: var SymTypeArgs,
) =
  discard
proc doAstEnum(
  args: var SymTypeArgs,
) =
  discard
proc doAstExtern(
  args: var SymTypeArgs,
) =
  discard
proc doAstCextern(
  args: var SymTypeArgs,
) =
  discard
proc doAstImport(
  args: var SymTypeArgs,
) =
  discard
proc doAstCImport(
  args: var SymTypeArgs,
) =
  discard
proc doAstScope(
  args: var SymTypeArgs,
) =
  discard
proc doAstIf(
  args: var SymTypeArgs,
) =
  discard
proc doAstElif(
  args: var SymTypeArgs,
) =
  discard
proc doAstElse(
  args: var SymTypeArgs,
) =
  discard
proc doAstSwitch(
  args: var SymTypeArgs,
) =
  discard
proc doAstCase(
  args: var SymTypeArgs,
) =
  discard
proc doAstDefault(
  args: var SymTypeArgs,
) =
  discard
proc doAstFor(
  args: var SymTypeArgs,
) =
  discard
proc doAstWhile(
  args: var SymTypeArgs,
) =
  discard
proc doAstContinue(
  args: var SymTypeArgs,
) =
  discard
proc doAstBreak(
  args: var SymTypeArgs,
) =
  discard
proc doAstReturn(
  args: var SymTypeArgs,
) =
  discard
proc doAstArray(
  args: var SymTypeArgs,
) =
  discard
proc doAstVoid(
  args: var SymTypeArgs,
) =
  discard
proc doAstBool(
  args: var SymTypeArgs,
) =
  discard
proc doAstU8(
  args: var SymTypeArgs,
) =
  discard
proc doAstI8(
  args: var SymTypeArgs,
) =
  discard
proc doAstU16(
  args: var SymTypeArgs,
) =
  discard
proc doAstI16(
  args: var SymTypeArgs,
) =
  discard
proc doAstU32(
  args: var SymTypeArgs,
) =
  discard
proc doAstI32(
  args: var SymTypeArgs,
) =
  discard
proc doAstU64(
  args: var SymTypeArgs,
) =
  discard
proc doAstI64(
  args: var SymTypeArgs,
) =
  discard
proc doAstF32(
  args: var SymTypeArgs,
) =
  discard
proc doAstF64(
  args: var SymTypeArgs,
) =
  discard
proc doAstChar(
  args: var SymTypeArgs,
) =
  discard
proc doAstString(
  args: var SymTypeArgs,
) =
  discard
proc doAstUnop(
  args: var SymTypeArgs,
) =
  discard
proc doAstBinop(
  args: var SymTypeArgs,
) =
  discard
proc doAstAssignEtc(
  args: var SymTypeArgs,
) =
  discard
proc doAstNamedType(
  args: var SymTypeArgs,
) =
  discard
proc doAstType(
  args: var SymTypeArgs,
) =
  discard
proc doAstFuncCall(
  args: var SymTypeArgs,
) =
  discard
proc doAstStmtExprLhs(
  args: var SymTypeArgs,
) =
  discard
proc doAstFuncNamedArgImpl(
  args: var SymTypeArgs,
) =
  discard
proc doAstGenericNamedArgImpl(
  args: var SymTypeArgs,
) =
  discard
proc doAstGenericList(
  args: var SymTypeArgs,
) =
  discard
proc doAstVarEtcDeclMost(
  args: var SymTypeArgs,
) =
  discard

proc doPassSymTypeMain(
  args: var SymTypeArgs
) =
  case args.ast.kind:
  of astSrcFile:
    # `module`: come back to this for when multiple Scone source files are
    # possible to be used in a single program!
    args.doAstSrcFile()
  of astIdent:
    args.doAstIdent()
  of astU64Lit:
    args.doAstU64Lit()
  of astStrLit:
    args.doAstStrLit()
  of astTrue:
    args.doAstTrue()
  of astFalse:
    args.doAstFalse()
  of astPtr:
    args.doAstPtr()
  of astDeref:
    args.doAstDeref()
  of astDot:
    args.doAstDot()
  of astVar:
    args.doAstVar()
  of astConst:
    args.doAstConst()
  of astDef:
    args.doAstDef()
  of astModule:
    args.doAstModule()
  of astStruct:
    args.doAstStruct()
  of astEnum:
    args.doAstEnum()
  of astExtern:
    args.doAstExtern()
  of astCextern:
    args.doAstCextern()
  of astImport:
    args.doAstImport()
  of astCImport:
    args.doAstCImport()
  of astScope:
    args.doAstScope()
  of astIf:
    args.doAstIf()
  of astElif:
    args.doAstElif()
  of astElse:
    args.doAstElse()
  of astSwitch:
    args.doAstSwitch()
  of astCase:
    args.doAstCase()
  of astDefault:
    args.doAstDefault()
  of astFor:
    args.doAstFor()
  of astWhile:
    args.doAstWhile()
  of astContinue:
    args.doAstContinue()
  of astBreak:
    args.doAstBreak()
  of astReturn:
    args.doAstReturn()
  of astArray:
    args.doAstArray()
  of astVoid:
    args.doAstVoid()
  of astBool:
    args.doAstBool()
  of astU8:
    args.doAstU8()
  of astI8:
    args.doAstI8()
  of astU16:
    args.doAstU16()
  of astI16:
    args.doAstI16()
  of astU32:
    args.doAstU32()
  of astI32:
    args.doAstI32()
  of astU64:
    args.doAstU64()
  of astI64:
    args.doAstI64()
  of astF32:
    args.doAstF32()
  of astF64:
    args.doAstF64()
  of astChar:
    args.doAstChar()
  of astString:
    args.doAstString()
  of astUnop:
    args.doAstUnop()
  of astBinop:
    args.doAstBinop()
  of astAssignEtc:
    args.doAstAssignEtc()
  of astNamedType:
    args.doAstNamedType()
  of astType:
    args.doAstType()
  of astFuncCall:
    args.doAstFuncCall()
  of astStmtExprLhs:
    args.doAstStmtExprLhs()
  of astFuncNamedArgImpl:
    args.doAstFuncNamedArgImpl()
  of astGenericNamedArgImpl:
    args.doAstGenericNamedArgImpl()
  of astGenericList:
    args.doAstGenericList()
  of astVarEtcDeclMost:
    args.doAstVarEtcDeclMost()


proc doPassSymType*(
  self: var Scone,
) =
  for subPass in SconeSubPassSymType(0) ..< limSpSymType:
    var myArgs = SymTypeArgs(
      self: addr self,
      subPass: subPass,
      ast: self.astRoot,
      parentSeq: @[],
    )
    doPassSymTypeMain(
      args=myArgs
    )
    #self.doPassSymTypeMain(
    #  ast=self.astRoot,
    #  parent=nil,
    #  subPass=subPass,
    #)
