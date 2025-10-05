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

proc doAstMain(
  #self: var Scone,
  args: SemMacArgs,
  ast: AstExpr,
) =
  proc doSubAstU64Lit(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myU64Lit = ast.myU64Lit
    discard
  proc doSubAstStrLit(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myStrLit = ast.myStrLit
    discard
  proc doSubAstOpenarrLit(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myOpenarrLit = ast.myOpenarrLit
    discard
  proc doSubAstTrue(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myTrue = ast.myTrue
    discard
  proc doSubAstFalse(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myFalse = ast.myFalse
    discard
  proc doSubAstDeref(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myDeref = ast.myDeref
    discard
  proc doSubAstDot(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myDot = ast.myDot
    discard
  proc doSubAstBuiltinTypeCast(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myBuiltinTypeCast = ast.myBuiltinTypeCast
    discard
  proc doSubAstExprIdent(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myExprIdent = ast.myExprIdent
    discard
  proc doSubAstUnop(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myUnop = ast.myUnop
    discard
  proc doSubAstBinop(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myBinop = ast.myBinop
    discard
  proc doSubAstFuncCall(
    args: SemMacArgs,
    ast: AstExpr,
  ) =
    #let myFuncCall = ast.myFuncCall
    discard

  case ast.kind:
  of exprU64Lit:
    args.doSubAstU64Lit(ast)
  of exprStrLit:
    args.doSubAstStrLit(ast)
  of exprOpenarrLit:
    args.doSubAstOpenarrLit(ast)
  of exprTrue:
    args.doSubAstTrue(ast)
  of exprFalse:
    args.doSubAstFalse(ast)
  of exprDeref:
    args.doSubAstDeref(ast)
  of exprDot:
    args.doSubAstDot(ast)
  of exprBuiltinTypeCast:
    args.doSubAstBuiltinTypeCast(ast)
  of exprExprIdent:
    args.doSubAstExprIdent(ast)
  of exprUnop:
    args.doSubAstUnop(ast)
  of exprBinop:
    args.doSubAstBinop(ast)
  of exprFuncCall:
    args.doSubAstFuncCall(ast)

proc doAstMain(
  args: SemMacArgs,
  ast: AstStmt,
) =
  proc doSubAstVar(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myVar = addr ast.myVar
    discard
  proc doSubAstConst(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myConst = addr ast.myConst
    discard
  proc doSubAstScope(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myScope = addr ast.myScope
    discard
  proc doSubAstIf(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myIf = addr ast.myIf
    discard
  proc doSubAstSwitch(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let mySwitch = addr ast.mySwitch
    discard
  proc doSubAstFor(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myFor = addr ast.myFor
    discard
  proc doSubAstWhile(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myWhile = addr ast.myWhile
    discard
  proc doSubAstContinue(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myContinue = addr ast.myContinue
    discard
  proc doSubAstBreak(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myBreak = addr ast.myBreak
    discard
  proc doSubAstReturn(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myReturn = addr ast.myReturn
    discard
  proc doSubAstAssignEtc(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myAssignEtc = addr ast.myAssignEtc
    discard
  proc doSubAstStmtExprLhs(
    args: SemMacArgs,
    ast: AstStmt,
  ) =
    #let myStmtExprLhs = addr ast.myStmtExprLhs
    discard

  case ast.kind:
  of stmtVar:
    args.doSubAstVar(ast)
  of stmtConst:
    args.doSubAstConst(ast)
  of stmtScope:
    args.doSubAstScope(ast)
  of stmtIf:
    args.doSubAstIf(ast)
  of stmtSwitch:
    args.doSubAstSwitch(ast)
  of stmtFor:
    args.doSubAstFor(ast)
  of stmtWhile:
    args.doSubAstWhile(ast)
  of stmtContinue:
    args.doSubAstContinue(ast)
  of stmtBreak:
    args.doSubAstBreak(ast)
  of stmtReturn:
    args.doSubAstReturn(ast)
  of stmtAssignEtc:
    args.doSubAstAssignEtc(ast)
  of stmtStmtExprLhs:
    args.doSubAstStmtExprLhs(ast)

proc doAstMain(
  args: SemMacArgs,
  ast: AstTypeSub
) =
  case ast.kind:
  of typeSubArray:
    discard
  of typeSubOpenarray:
    discard
  of typeSubBasicType:
    discard
  of typeSubNamedType:
    discard

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
