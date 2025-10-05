import std/tables
import std/sets
import std/options

import dataStructuresMisc
import ast
import scone

type
  SemMacArgs[T: HaveToAstNode] = ref object
    self*: ptr Scone
    #parentSeq*: seq[AstNode]
    parent*: T

template doAst[T: HaveToAstNode](
  childAst: T,
): untyped =
  let tempArgs = SemMacArgs(
    self: args.self,
    parent: ast,
  )
  tempArgs.doAstMain(childAst)
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
  #self: var Scone,
  args: SemMacArgs,
  ast: AstExpr,
) =
  case ast.kind:
  of exprU64Lit:
    discard
  of exprStrLit:
    discard
  of exprOpenarrLit:
    discard
  of exprTrue:
    discard
  of exprFalse:
    discard
  of exprDeref:
    discard
  of exprDot:
    discard
  of exprBuiltinTypeCast:
    discard
  of exprExprIdent:
    discard
  of exprUnop:
    discard
  of exprBinop:
    discard
  of exprFuncCall:
    discard

proc doAstMain(
  args: SemMacArgs,
  ast: AstStmt,
) =
  let self = addr args.self

proc doAstMain(
  args: SemMacArgs,
  ast: AstModule,
) =
  #let self = addr args.self
  let ident = doAst(ast.ident)

proc doAstMain(
  args: SemMacArgs,
  ast: AstSrcFile,
) =
  #let self = addr args.self
  doAst(ast.module)

proc doPassSemanticMacro*(
  self: var Scone,
) =
  #self.doAst(self.astRoot.mySrcFile)
  discard
