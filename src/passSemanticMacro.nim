import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import scone
import reduceEtc

type
  SemMacArgs = ref object
    self*: ptr Scone
    parentSeq*: ptr seq[AstNode]
    #parent*: AstNode
    #declPass*: uint
    semPass*: SconeSemanticPass
    macPass*: SconeMacroPass
    inSemPass*: bool
    didExpandMacro*: bool

template doAst(
  childAst: untyped,
): untyped =
  #let tempArgs = SemMacArgs(
  #  self: args.self,
  #  #parent: toAstNode(ast),
  #  parentSeq: args.parentSeq,
  #)
  args.parentSeq[].add toAstNode(ast)

  # NOTE: this `defer` evaluates entirely within this `template`
  defer: discard args.parentSeq[].pop()
  args.doAstMain(childAst)
  #args.doAstMain(childAst)

#proc myDoIt[T: HaveToAstNode](
#  args: SemMacArgs,
#  ast: ref T,
#) =
#  discard

proc doAstMain(
  args: SemMacArgs,
  ast: AstIdent,
): string =
  result = ast.strVal

proc doAstMain(
  args: SemMacArgs,
  ast: AstVarEtcDeclMost,
) =
  let ident = ast.ident.doAst()
#--------
proc doAstMain(
  #self: var Scone,
  args: SemMacArgs,
  ast: AstExpr,
) =
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstU64Lit,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstStrLit,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstOpenarrLit,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstTrue,
  ): bool =
    result = true
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstFalse,
  ): bool =
    result = false
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstDeref,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstDot,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstBuiltinTypeCast,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstExprIdent,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstUnop,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstBinop,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstFuncCall,
  ) =
    discard

  case ast.kind:
  of exprU64Lit:
    args.doSubAst(ast.myU64Lit)
  of exprStrLit:
    args.doSubAst(ast.myStrLit)
  of exprOpenarrLit:
    args.doSubAst(ast.myOpenarrLit)
  of exprTrue:
    discard args.doSubAst(ast.myTrue)
  of exprFalse:
    discard args.doSubAst(ast.myFalse)
  of exprDeref:
    args.doSubAst(ast.myDeref)
  of exprDot:
    args.doSubAst(ast.myDot)
  of exprBuiltinTypeCast:
    args.doSubAst(ast.myBuiltinTypeCast)
  of exprExprIdent:
    args.doSubAst(ast.myExprIdent)
  of exprUnop:
    args.doSubAst(ast.myUnop)
  of exprBinop:
    args.doSubAst(ast.myBinop)
  of exprFuncCall:
    args.doSubAst(ast.myFuncCall)

proc doAstMain(
  args: SemMacArgs,
  ast: AstStmt,
) =
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstVar,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstConst,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstScope,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstIf,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstSwitch,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstFor,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstWhile,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstContinue,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstBreak,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstReturn,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstAssignEtc,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstStmtExprLhs,
  ) =
    discard
  case ast.kind:
  of stmtVar:
    args.doSubAst(ast.myVar)
  of stmtConst:
    args.doSubAst(ast.myConst)
  of stmtScope:
    args.doSubAst(ast.myScope)
  of stmtIf:
    args.doSubAst(ast.myIf)
  of stmtSwitch:
    args.doSubAst(ast.mySwitch)
  of stmtFor:
    args.doSubAst(ast.myFor)
  of stmtWhile:
    args.doSubAst(ast.myWhile)
  of stmtContinue:
    args.doSubAst(ast.myContinue)
  of stmtBreak:
    args.doSubAst(ast.myBreak)
  of stmtReturn:
    args.doSubAst(ast.myReturn)
  of stmtAssignEtc:
    args.doSubAst(ast.myAssignEtc)
  of stmtStmtExprLhs:
    args.doSubAst(ast.myStmtExprLhs)

proc doAstMain(
  args: SemMacArgs,
  ast: AstTypeSub
) =
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstArray,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstOpenarray,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstBasicType,
  ) =
    discard
  proc doSubAst(
    args: SemMacArgs,
    ast: var SubAstNamedType,
  ) =
    discard

  let parent = args.parentSeq[][^1].myType

  case ast.kind:
  of typeSubArray:
    args.doSubAst(ast.myArray)
  of typeSubOpenarray:
    args.doSubAst(ast.myOpenarray)
  of typeSubBasicType:
    args.doSubAst(ast.myBasicType)
  of typeSubNamedType:
    args.doSubAst(ast.myNamedType)

proc doAstMain(
  args: SemMacArgs,
  ast: AstModule,
) =
  #let self = addr args.self
  #let ident = doAst(ast.ident)
  #echo sconcat(@[
  #  "module ", ident
  #])

  # TODO: support multiple source files/modules
  discard

proc doAstMain(
  args: SemMacArgs,
  ast: AstDef,
) =
  let ident = ast.ident.doAst()
  #echo sconcat(@[
  #  "def ", ident
  #])
  #if args.inSemPass and args.semPass == scoSemPassDecls:
  #  discard
  if args.inSemPass:
    case args.semPass:
    else:
      discard
  else:
    discard


  #if args.declPass == 0u32:
  #  discard
  #else:
  #  discard

proc doAstMain(
  args: SemMacArgs,
  ast: AstStruct,
) =
  let ident = ast.ident.doAst()
  #echo sconcat(@[
  #  "struct ", ident
  #])
  if args.inSemPass and args.semPass == scoSemPassDecls:
    discard
  #if args.declPass == 0u32:
  #  discard
  #else:
  #  discard

proc doAstMain(
  args: SemMacArgs,
  ast: AstSrcFile,
) =
  #let self = addr args.self
  ast.module.doAst()
  for funcDecl in ast.funcDeclSeq:
    funcDecl.doAst()
  for structDecl in ast.structDeclSeq:
    structDecl.doAst()

proc doPassSemanticMacro*(
  self: var Scone,
) =
  #self.doAst(self.astRoot.mySrcFile)
  var parentSeq: seq[AstNode]
  var args = SemMacArgs(
    self: addr self,
    parentSeq: addr parentSeq,
    #declPass: 0u32,
    semPass: SconeSemanticPass(0),
    macPass: SconeMacroPass(0),
    didExpandMacro: false
  )

  var ast = self.astRoot.mySrcFile

  while true:
    #if args.declPass > 0u32:
    #  args.didExpandMacro = false

    args.inSemPass = orR(@[
      args.macPass == scoMacroPassSemFirst,
      args.macPass == scoMacroPassSemSecond,
    ])

    ast.doAst()

    if args.inSemPass:
      args.semPass = SconeSemanticPass(uint32(args.semPass) + 1u32)
      if args.semPass == limScoSemPass:
        args.macPass = SconeMacroPass(uint32(args.macPass) + 1u32)
    else:
      args.semPass = SconeSemanticPass(0u32)
      args.macPass = SconeMacroPass(uint32(args.macPass) + 1u32)

    if args.macPass == limScoMacroPass:
      if args.didExpandMacro:
        args.didExpandMacro = false
        args.semPass = SconeSemanticPass(0u32)
        args.macPass = SconeMacroPass(0u32)
      else:
        break
        

    #if args.declPass == 0u32:
    #  args.declPass += 1u32
    #elif args.didExpandMacro:
    #  # check decls again
    #  args.declPass = 0u32
