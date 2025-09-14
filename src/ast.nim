import std/options
import std/macros
import std/strutils

import dataStructuresMisc
#import symType

import reduceEtc


proc tokToUnop*(
  tok: TokKind,
): Option[AstUnopKind] =
  result = none(AstUnopKind)

  case tok:
  of tokPlus:
    return some(unopPlus)
  of tokMinus:
    return some(unopMinus)
  of tokLogicNot:
    return some(unopLogicNot)
  of tokBitInvert:
    return some(unopBitInvert)
  of tokAddr:
    return some(unopAddr)
  else:
    return none(AstUnopKind)
proc unopToTok*(
  unop: AstUnopKind,
): TokKind =
  case unop:
  of unopPlus:
    return tokPlus
  of unopMinus:
    return tokMinus
  of unopLogicNot:
    return tokLogicNot
  of unopBitInvert:
    return tokBitInvert
  of unopAddr:
    return tokAddr

proc tokToBinop*(
  tok: TokKind,
): Option[AstBinopKind] =
  result = none(AstBinopKind)
  case tok:
  of tokCmpEq:
    return some(binopCmpEq)
  of tokCmpNe:
    return some(binopCmpNe)
  of tokCmpLt:
    return some(binopCmpLt)
  of tokCmpGt:
    return some(binopCmpGt)
  of tokCmpLe:
    return some(binopCmpLe)
  of tokCmpGe:
    return some(binopCmpGe)
  #--------
  of tokPlus:
    return some(binopPlus)
  of tokMinus:
    return some(binopMinus)
  of tokMul:
    return some(binopMul)
  of tokDiv:
    return some(binopDiv)
  of tokMod:
    return some(binopMod)
  of tokBitAnd:
    return some(binopBitAnd)
  of tokBitOr:
    return some(binopBitOr)
  of tokBitXor:
    return some(binopBitXor)
  of tokLogicAnd:
    return some(binopLogicAnd)
  of tokLogicOr:
    return some(binopLogicOr)
  of tokBitShl:
    return some(binopBitShl)
  of tokBitShr:
    return some(binopBitShr)
  else:
    return none(AstBinopKind)

proc binopToTok*(
  binop: AstBinopKind,
): TokKind =
  case binop:
  of binopCmpEq:
    return tokCmpEq
  of binopCmpNe:
    return tokCmpNe
  of binopCmpLt:
    return tokCmpLt
  of binopCmpGt:
    return tokCmpGt
  of binopCmpLe:
    return tokCmpLe
  of binopCmpGe:
    return tokCmpGe
  #--------
  of binopPlus:
    return tokPlus
  of binopMinus:
    return tokMinus
  of binopMul:
    return tokMul
  of binopDiv:
    return tokDiv
  of binopMod:
    return tokMod
  of binopBitAnd:
    return tokBitAnd
  of binopBitOr:
    return tokBitOr
  of binopBitXor:
    return tokBitXor
  of binopLogicAnd:
    return tokLogicAnd
  of binopLogicOr:
    return tokLogicOr
  of binopBitShl:
    return tokBitShl
  of binopBitShr:
    return tokBitShr

proc assignEtcToTok*(
  assignEtc: AstAssignEtcKind,
): TokKind =
  case assignEtc:
  of assignEtcRegular:
    return tokAssign
  of assignEtcPlus:
    return tokAssignPlus
  of assignEtcMinus:
    return tokAssignMinus
  of assignEtcMul:
    return tokAssignMul
  of assignEtcDiv:
    return tokAssignDiv
  of assignEtcMod:
    return tokAssignMod
  of assignEtcBitAnd:
    return tokAssignBitAnd
  of assignEtcBitOr:
    return tokAssignBitOr
  of assignEtcBitXor:
    return tokAssignBitXor
  of assignEtcBitShl:
    return tokAssignBitShl
  of assignEtcBitShr:
    return tokAssignBitShr

proc tokToAssignEtc*(
  tok: TokKind,
): Option[AstAssignEtcKind] =
  result = none(AstAssignEtcKind)

  case tok:
  of tokAssign:
    return some(assignEtcRegular)
  of tokAssignPlus:
    return some(assignEtcPlus)
  of tokAssignMinus:
    return some(assignEtcMinus)
  of tokAssignMul:
    return some(assignEtcMul)
  of tokAssignDiv:
    return some(assignEtcDiv)
  of tokAssignMod:
    return some(assignEtcMod)
  of tokAssignBitAnd:
    return some(assignEtcBitAnd)
  of tokAssignBitOr:
    return some(assignEtcBitOr)
  of tokAssignBitXor:
    return some(assignEtcBitXor)
  of tokAssignBitShl:
    return some(assignEtcBitShl)
  of tokAssignBitShr:
    return some(assignEtcBitShr)
  else:
    return none(AstAssignEtcKind)

proc mkPubIdent(
  name: string
): NimNode =
  result = add(
    add(newNimNode(nnkPostfix),
      ident("*"), ident(name)
    )
  )

macro mkAstHier(): untyped =
  #result = newTypeSection(
  #)
  #result = quote do:
  #  discard
  result = newNimNode(nnkTypeSection)
  #var typeDefSeq: seq[NimNode]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if not h[2]:
      continue

    var typeDef = newNimNode(nnkTypeDef)

    typeDef.add mkPubIdent("Ast" & h[0])
    typeDef.add newNimNode(nnkEmpty)
    #var objTyArgs: seq[NimNode]
    #objTyArgs

    var objTy = add(
      newNimNode(nnkObjectTy),
      [
        newNimNode(nnkEmpty),
        newNimNode(nnkEmpty),
        #newNimNode(nnkRecList),
      ]
    )
    #objTy.dumpTree()

    #for arg in objTyArgs:
    #  objTy.add arg
    template recList(): untyped = objTy[2]
    var didAddRecList: bool = false
    for jdx in 0 ..< h[3].len():
      if jdx == 0:
        didAddRecList = true
        objTy.add(newNimNode(nnkRecList))
      let field = h[3][jdx]
      #echo "jdx:" & $jdx & " " & $h
      #echo "field:" & $field

      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add mkPubIdent(field[0])
      #echo identDefs.repr()

      case field[1]:
      of astValAstNode:
        identDefs.add ident("AstNode")
      of astValSeqAstNode:
        identDefs.add(
          add(
            newNimNode(nnkBracketExpr),
            ident("seq"),
            ident("AstNode"),
          )
        )
      of astValOptAstNode:
        identDefs.add(
          add(newNimNode(nnkBracketExpr),
            ident("Option"),
            ident("AstNode"),
          )
        )
      of astValString:
        identDefs.add ident("string")
      of astValU64:
        identDefs.add ident("uint64")
      of astValBool:
        identDefs.add ident("bool")
      of astValUnopKind:
        identDefs.add ident("AstUnopKind")
      of astValBinopKind:
        identDefs.add ident("AstBinopKind")
      of astValAssignEtcKind:
        identDefs.add ident("AstAssignEtcKind")
      of astValBasicTypeKind:
        identDefs.add ident("AstBasicTypeKind")

      identDefs.add newNimNode(nnkEmpty)
      recList.add identDefs
    if not didAddRecList:
      objTy.add newNimNode(nnkEmpty)
    typeDef.add objTy

    result.add typeDef

  block:
    var typeDef = newNimNode(nnkTypeDef)
    typeDef.add mkPubIdent("AstNode")
    typeDef.add newNimNode(nnkEmpty)
    typeDef.add(
      add(newNimNode(nnkRefTy), ident("AstNodeObj"))
    )
    result.add typeDef
    #echo result.repr()

  block:
    var typeDef = newNimNode(nnkTypeDef)
    typeDef.add mkPubIdent("AstNodeObj")
    typeDef.add newNimNode(nnkEmpty)
    var objTy = newNimNode(nnkObjectTy)
    template recList(): untyped = objTy[2]
    objTy.add(
      newNimNode(nnkEmpty), newNimNode(nnkEmpty), newNimNode(nnkRecList)
    )
    block:
      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add(
        mkPubIdent("lexMain"),
        ident("LexMain"),
        newNimNode(nnkEmpty),
      )
      recList.add identDefs
    #block:
    #  var identDefs = newNimNode(nnkIdentDefs)
    #  identDefs.add(
    #    mkPubIdent("parentExpr"),
    #    ident("AstNode"),
    #    newNimNode(nnkEmpty),
    #  )
    #  recList.add identDefs
    block:
      var recCase = newNimNode(nnkRecCase)
      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add(
        mkPubIdent("kind"),
        ident("AstKind"),
        newNimNode(nnkEmpty),
      )
      recCase.add identDefs
      for idx in 0 ..< helperTokKindSeq.len():
        let h = helperTokKindSeq[idx]
        if not h[2]:
          continue

        var ofBranch = newNimNode(nnkOfBranch)
        ofBranch.add(
          ident("ast" & h[0])
        )
        var obIdentDefs = newNimNode(nnkIdentDefs)
        obIdentDefs.add(
          mkPubIdent("my" & h[0]),
          ident("Ast" & h[0]),
          newNimNode(nnkEmpty)
        )

        ofBranch.add obIdentDefs
        recCase.add ofBranch
      recList.add recCase

    #objTy.add 

    typeDef.add objTy
    result.add typeDef

    #echo result.repr()

  #for idx in 0 ..< helperTokKindSeq.len():
  #  let h = helperTokKindSeq[idx]
  #  if not h[2]:
  #    continue


mkAstHier()

proc doIndent*(
  indent: uint,
): string =
  for idx in 0 ..< indent:
    result.add " "

#macro toStr*(
#  ast: AstNode
#): untyped =
#  #dumpTree(ast)
#  case ast.kind:
#  of 
#  discard

proc toStr*(
  astSeq: seq[AstNode],
  indent: uint,
): string

proc toStr*(
  ast: AstNode,
  indent: uint,
): string

#CaseStmt
#  DotExpr
#    Ident "ast"
#    Ident "kind"
#  OfBranch
#    Ident "astSrcFile"
#    StmtList
#      Command
#        DotExpr
#          Ident "result"
#          Ident "add"
#        StrLit "(AstSrcFile\n"
#      Call
#        DotExpr
#          Ident "result"
#          Ident "add"
#        Infix
#          Ident "&"
#          Infix
#            Ident "&"
#            Infix
#              Ident "&"
#              Ident "i"
#              StrLit "(module "
#            Call
#              DotExpr
#                DotExpr
#                  DotExpr
#                    Ident "ast"
#                    Ident "mySrcFile"
#                  Ident "module"
#                Ident "toStr"
#              Ident "x"
#          StrLit ")\n"


#            Call
#              DotExpr
#                Ident "result"
#                Ident "add"
#              Infix
#                Ident "&"
#                Infix
#                  Ident "&"
#                  Infix
#                    Ident "&"
#                    Ident "i"
#                    StrLit "(structDeclSeq "
#                  Call
#                    DotExpr
#                      DotExpr
#                        DotExpr
#                          Ident "ast"
#                          Ident "mySrcFile"
#                        Ident "structDeclSeq"
#                      Ident "toStr"
#                    Ident "x"
#                StrLit ")\n"


proc toStr*(
  ast: AstNode,
  indent: uint,
): string =
  if ast == nil:
    return "nil"
  var x = indent + 2
  #result.add doIndent(indent=indent)
  template i(): untyped =
    doIndent(indent=x)
  #let iFinish = i & ")\n"
  var iFinish = doIndent(indent=indent)

  macro doCaseStmt(): untyped =
    result = newNimNode(nnkCaseStmt)
    result.add(
      add(newNimNode(nnkDotExpr), ident("ast"), ident("kind"))
    )
    #echo result.repr()
    for idx in 0 ..< helperTokKindSeq.len():
      let h = helperTokKindSeq[idx]
      if not h[2]:
        continue

      var ofBranch = newNimNode(nnkOfBranch)
      ofBranch.add ident("ast" & h[0])
      var stmtList = newNimNode(nnkStmtList)

      var myCmd0 = newNimNode(nnkCommand)
      let tempAstName = "(Ast" & h[0]
      let tempNimNode = quote do:
        #i & 
        `tempAstName`
        
      myCmd0.add(
        add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
        #newLit(
        #  "(Ast" & h[0] #& "\n" #& i
        #),
        tempNimNode
      )
      stmtList.add myCmd0
      #stmtList.add quote do:
      #  result.add i

      var tempNl: NimNode
      var tempI: NimNode
        

      for jdx in 0 ..< h[3].len():
        let field = h[3][jdx]
        if jdx == 0 and h[3].len > 1:
          stmtList.add quote do:
            result.add "\n" #`tempNl`
        #else:
        if h[3].len <= 1:
          stmtList.add quote do:
            x = indent
        if h[3].len() > 1:
          #tempNl = "\n"
          tempNl = quote do:
            "\n" #& i
          tempI = quote do:
            i
        else:
          tempNl = quote do:
            #i
            ""
          tempI = quote do:
            " "
        #var myCmdInner = newNimNode(nnkCommand)

        #proc myAddIndent(myCmdInner: var NimNode) =
        #  myCmdInner.add(
        #    add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
        #    ident("i")
        #  )
        #myCmdInner.myAddIndent()

        #var myDotExpr = (
        #  add(newNimNode(nnkDotExpr), ident("result"), ident("add"))
        #)
        var toAdd: NimNode = nil

        #let myResultIdent = ident("result")
        #let myAddIdent = ident("add")
        #let myIIdent = ident("i")
        #let myXIdent = ident("x")
        let myAstIdent = ident("ast")
        let myMbrIdent = ident("my" & h[0])
        let mbrStr = newLit(field[0])
        let myInnerMbrIdent = ident(field[0])
        #let myToStrIdent = ident("toStr")
        let myDualDotExpr = quote do:
          `myAstIdent`.`myMbrIdent`.`myInnerMbrIdent`

        case field[1]:
        of astValAstNode:
          toAdd = quote do:
            result.add(
              (
                #"\n" & i & 
                #`tempNl` & 
                `tempI` & `mbrStr` & " "
              ) & (
                `myDualDotExpr`.toStr(x) #& i & ")\n"
              ) & (
                `tempNl`
              )
            )
        of astValSeqAstNode:
          toAdd = quote do:
            result.add(
              (
                #"\n" & i & 
                #`tempNl` & 
                `tempI` & `mbrStr` & " "
              ) & (
                `myDualDotExpr`.toStr(x) #& "\n"
              ) & (
                `tempNl`
              )
            )
        of astValOptAstNode:
          toAdd = quote do:
            if `myDualDotExpr`.isSome:
              result.add(
                (
                  #"\n" & i & 
                  #`tempNl` & 
                  `tempI` & `mbrStr` & " "
                ) & (
                  `myDualDotExpr`.get.toStr(x)
                ) & (
                  `tempNl`
                )
              )
            else:
              result.add(
                #"\n" & i & 
                (
                  `tempI` & `mbrStr` & " " & "!isSome" #& ")\n"
                ) & (
                  `tempNl`
                )
              )
        of astValString:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              `tempI` & "\"" & `myDualDotExpr` & "\"" & `tempNl`
            )
        of astValU64:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValBool:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValUnopKind:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              #" " & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValBinopKind:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              #" " & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValAssignEtcKind:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              #" " & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValBasicTypeKind:
          toAdd = quote do:
            result.add(
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )

        #myCmdInner.add myDotExpr
        #myCmdInner.add toAdd

        stmtList.add toAdd
      #if h[3].len() == 0:
      block:
        var toAdd: NimNode = nil
        if orR(@[
          h[3].len() == 0,
          h[3].len() == 1,
        ]):
          toAdd = quote do:
            result.add(")")
        else:
          toAdd = quote do:
            result.add(iFinish & ")")
        stmtList.add toAdd

      #var myCmd1 = newNimNode(nnkCommand)
      #myCmd1.add(
      #  add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
      #  #newLit(")\n"),
      #  ident("iFinish"),
      #)
      #stmtList.add myCmd1

      ofBranch.add stmtList
      result.add ofBranch
    #dumpTree result
    #echo "<-- -->"

    #echo result.repr()
  doCaseStmt()

  #case ast.kind:
  #of astSrcFile:
  #  result.add "(AstSrcFile\n"
  #  result.add(
  #    i & "(module " & ast.mySrcFile.module.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(funcDeclSeq " & ast.mySrcFile.funcDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(structDeclSeq " & ast.mySrcFile.structDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add ")\n"
  #of astIdent:
  #  result.add "(AstIdent " & ast.myIdent.strVal & ") "
  #of astU64Lit:
  #  result.add "(AstU64Lit " & $ast.myU64Lit.u64Val & ") "
  #of astStrLit:
  #  result.add "(AstStrLit \"" & ast.myStrLit.strLitVal & "\") "
  #of astTrue:
  #  result.add "(" & helperTokKindSeq[uint(tokTrue)][1].get() & ") "
  #of astFalse:
  #  result.add "(" & helperTokKindSeq[uint(tokFalse)][1].get() & ") "
  #of astPtr:
  #  result.add "(" & helperTokKindSeq[uint(tokPtr)][1].get() & ") "
  #of astAddr:
  #  result.add "(" & helperTokKindSeq[uint(tokAddr)][1].get() & ") "
  #of astDeref:
  #  result.add "(" & helperTokKindSeq[uint(tokDeref)][1].get() & ") "
  #of astDot:
  #  result.add "(" & helperTokKindSeq[uint(tokDot)][1].get() & ") "
  #of astVar:
  #  result.add "(AstVar\n"
  #  result.add(
  #    i & "(ident " & ast.myVar.ident.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(myType " & ast.myVar.myType.toStr(x) & ")\n"
  #  )
  #  if ast.myVar.optExpr.isSome:
  #    result.add(
  #      i & "(optExpr " & ast.myVar.optExpr.get().toStr(x) & ")\n"
  #    )
  #  else:
  #    result.add(
  #      i & "(not optExpr.isSome)\n"
  #    )
  #  result.add ")\n"
  #of astConst:
  #  result.add "(AstConst\n"
  #  result.add(
  #    i & "(ident " & ast.myConst.ident.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(myType " & ast.myConst.myType.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(expr " & ast.myConst.expr.toStr(x) & ")\n"
  #  )
  #  result.add ")\n"
  #of astDef:
  #  result.add "(AstDef\n"
  #  result.add(
  #    i & "(ident " & ast.myDef.ident.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(genericDeclSeq " & ast.myDef.genericDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(argDeclSeq " & ast.myDef.argDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add ")\n"
  #of astModule:
  #  result.add "(AstModule\n"
  #  result.add ast.myModule.ident.toStr(x)
  #  result.add ")\n"
  #else:
  #  result.add "(Other)"

proc toStr*(
  astSeq: seq[AstNode],
  indent: uint,
): string =
  let x = indent + 2
  let tempIndent = doIndent(indent=indent)
  let tempIndent1 = doIndent(indent=x)
  #result.add tempIndent
  #let i = doIndent(ident=x)

  result.add "[\n"
  for idx in 0 ..< astSeq.len():
    result.add tempIndent1 & astSeq[idx].toStr(x) & "\n"
  #if int(indent - 2) > 0:
  #  result.add doIndent(indent - 2)
  result.add tempIndent
  result.add "]"

#proc repr*(
#  ast: AstNode,
#  indent: uint,
#): string =
#  discard
  
proc `$`*(
  ast: AstNode
): string =
  result = ast.toStr(0)

proc toRepr*(
  ast: AstNode,
  parent: AstNode=nil,
  indent: int=(-2),
  #level: int=0,
): string =
  proc myToRepr(
    otherAst: AstNode,
    indent: int=(-2)
  ): string =
    result = otherAst.toRepr(parent=ast, indent=indent)

  if ast == nil:
    return "(eek! `nil`)"
  var x = indent + 2
  template i(): untyped =
    doIndent(indent=uint(x))
  var iPrev: Option[string] = none(string)
  if indent >= 0:
    iPrev = some(doIndent(indent=uint(indent)))

  proc helperStmtSeq(
    stmtSeq: seq[AstNode],
    toSub: int=0,
    #includeSemicolon: bool=true,
  ): string =
    for idx in 0 ..< stmtSeq.len():
      result.add stmtSeq[idx].myToRepr(x - toSub) & "\n"
      #if includeSemicolon:
      #  result.add ";"
      #result.add "\n"
    

  case ast.kind:
  of astSrcFile:
    result.add ast.mySrcFile.module.myToRepr() & ";\n\n"
    for idx in 0 ..< ast.mySrcFile.funcDeclSeq.len():
      result.add ast.mySrcFile.funcDeclSeq[idx].myToRepr() & ";\n\n"
    for idx in 0 ..< ast.mySrcFile.structDeclSeq.len():
      result.add ast.mySrcFile.structDeclSeq[idx].myToRepr() & ";\n\n"
  of astIdent:
    result.add ast.myIdent.strVal
  of astU64Lit:
    result.add $ast.myU64Lit.u64Val
  of astStrLit:
    result.add "\"" & ast.myStrLit.strLitVal & "\""
  of astTrue:
    result.add "true"
  of astFalse:
    result.add "false"
  #of astPtr:
  #  result.add "ptr"
  #of astAddr:
  #  result.add "(" & "addr " & ast.myAddr.obj.myToRepr(x) & ")"
  of astDeref:
    #result.add "(" & ast.myDeref.obj.myToRepr(x) & "@)"
    result.add ast.myDeref.obj.myToRepr(x) & "@"
  of astDot:
    #result.add "("
    result.add ast.myDot.left.myToRepr(x) & "."
    result.add ast.myDot.right.myToRepr(x)
    #result.add ")"
  of astVar:
    result.add i & "var " & ast.myVar.child.myToRepr()
    if ast.myVar.optExpr.isSome:
      result.add " = " & ast.myVar.optExpr.get().myToRepr()
    result.add ";"
  of astConst:
    result.add i & "const " & ast.myConst.child.myToRepr()
    result.add " = " & ast.myConst.expr.myToRepr()
    result.add ";"
  of astDef:
    result.add "def " & ast.myDef.ident.myToRepr()
    if ast.myDef.genericDecl.isSome:
      result.add ast.myDef.genericDecl.get().myToRepr()
    result.add "("
    #result.add ast.myDef.argDeclSeq
    for idx in 0 ..< ast.myDef.argDeclSeq.len():
      result.add ast.myDef.argDeclSeq[idx].myToRepr()
      if idx + 1 < ast.myDef.argDeclSeq.len():
        result.add ", "
    result.add ") -> " & ast.myDef.returnType.myToRepr() & " {\n"
    #for idx in 0 ..< ast.myDef.stmtSeq.len():
    #  result.add i & ast.myDef.stmtSeq[idx].myToRepr(x) & ";\n"
    result.add helperStmtSeq(ast.myDef.stmtSeq)
    result.add "}"
  of astModule:
    result.add "module " & ast.myModule.ident.myToRepr()
  of astStruct:
    result.add "struct " & ast.myStruct.ident.myToRepr()
    if ast.myStruct.genericDecl.isSome:
      result.add ast.myStruct.genericDecl.get().myToRepr() & " {\n"
    for idx in 0 ..< ast.myStruct.fieldSeq.len():
      result.add(
        (
          doIndent(uint(x + 2))
        ) & (
          ast.myStruct.fieldSeq[idx].myToRepr() & ";\n"
        )
      )
    result.add "}"
  of astEnum:
    discard
  of astExtern:
    discard
  of astCextern:
    discard
  of astImport:
    discard
  of astCImport:
    discard
  of astScope:
    result.add iPrev.get() & "scope {\n"
    result.add helperStmtSeq(ast.myScope.stmtSeq)
    result.add iPrev.get() & "}"
  of astIf:
    result.add (
      i & "if " & ast.myIf.expr.myToRepr() & " {\n"
    )
    result.add helperStmtSeq(ast.myIf.stmtSeq)
    result.add i & "}"
    #if ast.myIf.elifSeq.len() > 0:
    #  result.add ast.myIf.elifSeq[0].myToRepr(x)
    #  #if ast.myIf.optElse.isSome:
    #  #  result.add " " & ast.myIf.optElse.get().myToRepr(x)
    #for myElif in ast.myIf.elifSeq:
    for idx in 0 ..< ast.myIf.elifSeq.len():
      result.add " " & ast.myIf.elifSeq[idx].myToRepr(x - 2)

    if ast.myIf.optElse.isSome:
      result.add " " & ast.myIf.optElse.get().myToRepr(x - 2)
  of astElif:
    result.add "elif " & ast.myElif.expr.myToRepr() & " {\n"
    result.add helperStmtSeq(ast.myElif.stmtSeq)
    result.add i & "}"
    discard
  of astElse:
    result.add "else {\n"
    result.add helperStmtSeq(ast.myElse.stmtSeq)
    result.add i & "}"
  of astSwitch:
    result.add i & "switch " & ast.mySwitch.expr.myToRepr() & " {\n"
    result.add helperStmtSeq(ast.mySwitch.caseSeq)
    if ast.mySwitch.optDefault.isSome:
      result.add i & ast.mySwitch.optDefault.get().myToRepr(x - 2)
      result.add "\n"
    result.add i & "}"
  of astCase:
    result.add iPrev.get() & "case " & ast.myCase.expr.myToRepr() & " {\n"
    result.add helperStmtSeq(ast.myCase.stmtSeq)
    result.add iPrev.get() & "}"
  of astDefault:
    result.add "default {\n"
    result.add helperStmtSeq(ast.myDefault.stmtSeq)
    result.add i & "}"
  of astFor:
    result.add i & "for "
    result.add ast.myFor.ident.myToRepr() & " in "
    result.add ast.myFor.exprPre.myToRepr()
    if ast.myFor.isUntil:
      result.add " until "
    else:
      result.add " to "
    result.add ast.myFor.exprPost.myToRepr() & " {\n"
    result.add helperStmtSeq(ast.myFor.stmtSeq)
    result.add i & "}"
  of astWhile:
    result.add i & "while "
    result.add ast.myWhile.expr.myToRepr() & " {\n"
    result.add helperStmtSeq(ast.myWhile.stmtSeq)
    result.add i & "}"
  of astContinue:
    result.add i & "continue" & ";"
  of astBreak:
    result.add i & "break" & ";"
  of astReturn:
    result.add i & "return"
    if ast.myReturn.optExpr.isSome:
      result.add " " & ast.myReturn.optExpr.get().myToRepr(x)
    result.add ";"
  of astArray:
    result.add "array["
    result.add ast.myArray.dim.myToRepr() & "; "
    result.add ast.myArray.elemType.myToRepr()
    result.add "]"
  of astUnop:
    var inclParens: bool = false
    let myUnopExprOp = (
      AstExprOp(
        kind: exprOpUnop,
        myUnop: ast.myUnop.kind,
      )
    )
    if parent != nil:
      var myObjExprOp: AstExprOp
      case parent.kind:
      of astUnop:
        #inclParens = false
        #myObjExprOp.kind = exprOpUnop
        #myObjExprOp.myUnop = parent.myUnop.kind
        #inclParens = myUnopExprOp.cmpPrioLt(myObjExprOp)
        inclParens = true
      of astBinop:
        #inclParens = false
        myObjExprOp.kind = exprOpBinop
        myObjExprOp.myBinop = parent.myBinop.kind
        inclParens = myUnopExprOp.cmpPrioLt(myObjExprOp)
      else:
        inclParens = false
        discard

    if inclParens:
      result.add "("
    result.add(
      helperTokKindSeq[uint(ast.myUnop.kind.unopToTok())][1].get()
    )
    if ast.myUnop.kind == unopAddr:
      result.add " "
    result.add ast.myUnop.obj.myToRepr()
    if inclParens:
      result.add ")"
  of astBinop:
    var inclParens: bool = false
    let myBinopExprOp = (
      AstExprOp(
        kind: exprOpBinop,
        myBinop: ast.myBinop.kind,
      )
    )
    if parent != nil:
      var myObjExprOp: AstExprOp
      case parent.kind:
      of astUnop:
        #inclParens = false
        myObjExprOp.kind = exprOpUnop
        myObjExprOp.myUnop = parent.myUnop.kind
        inclParens = myBinopExprOp.cmpPrioLt(myObjExprOp)
      of astBinop:
        #inclParens = false
        myObjExprOp.kind = exprOpBinop
        myObjExprOp.myBinop = parent.myBinop.kind
        inclParens = myBinopExprOp.cmpPrioLt(myObjExprOp)
      else:
        inclParens = false
        discard

    if inclParens:
      result.add "("
    result.add ast.myBinop.left.myToRepr() & " "
    result.add(
      helperTokKindSeq[uint(ast.myBinop.kind.binopToTok())][1].get()
    )
    result.add " " & ast.myBinop.right.myToRepr()
    if inclParens:
      result.add ")"
  of astAssignEtc:
    #result.add "("
    result.add i & ast.myAssignEtc.left.myToRepr() & " "
    result.add(
      helperTokKindSeq[
        uint(ast.myAssignEtc.kind.assignEtcToTok())
      ][1].get()
    )
    result.add " " & ast.myAssignEtc.right.myToRepr()
    #result.add ")"
    result.add ";"
  of astBasicType:
    case ast.myBasicType.kind:
    of basicTypeVoid:
      result.add "void"
    of basicTypeBool:
      result.add "bool"
    of basicTypeU8:
      result.add "u8"
    of basicTypeI8:
      result.add "i8"
    of basicTypeU16:
      result.add "u16"
    of basicTypeI16:
      result.add "i16"
    of basicTypeU32:
      result.add "u32"
    of basicTypeI32:
      result.add "i32"
    of basicTypeU64:
      result.add "u64"
    of basicTypeI64:
      result.add "i64"
    of basicTypeF32:
      result.add "f32"
    of basicTypeF64:
      result.add "f64"
    of basicTypeChar:
      result.add "char"
    of basicTypeString:
      result.add "string"
  of astNamedType:
    result.add ast.myNamedType.ident.myToRepr()
    result.add ast.myNamedType.genericImpl.myToRepr()
  of astType:
    if ast.myType.kwVar:
      doAssert(
        ast.myType.ptrDim == 0,
        "eek! " & $ast
      )
      result.add "var "
    if ast.myType.ptrDim > 0:
      doAssert(
        not ast.myType.kwVar,
        "eek! " & $ast
      )
      for idx in 0 ..< ast.myType.ptrDim:
        result.add "ptr "
    result.add ast.myType.child.myToRepr()
  of astFuncCall:
    result.add ast.myFuncCall.ident.myToRepr()
    result.add ast.myFuncCall.genericImpl.myToRepr()
    result.add "("
    for idx in 0 ..< ast.myFuncCall.argImplSeq.len():
      result.add ast.myFuncCall.argImplSeq[idx].myToRepr()
      if idx + 1 < ast.myFuncCall.argImplSeq.len():
        result.add ", "
    result.add ")"
  of astStmtExprLhs:
    result.add i & ast.myStmtExprLhs.expr.myToRepr() & ";"
  #of astStmtFuncCall:
  #  result.add i & ast.myStmtFuncCall.funcCall.myToRepr() & ";"
  of astFuncNamedArgImpl:
    result.add ast.myFuncNamedArgImpl.ident.myToRepr() & "="
    result.add ast.myFuncNamedArgImpl.expr.myToRepr()
  of astGenericNamedArgImpl:
    result.add ast.myGenericNamedArgImpl.ident.myToRepr() & "="
    result.add ast.myGenericNamedArgImpl.type.myToRepr()
  of astGenericList:
    if ast.myGenericList.mySeq.len() > 0:
      result.add "["
      for idx in 0 ..< ast.myGenericList.mySeq.len():
        result.add ast.myGenericList.mySeq[idx].myToRepr()
        if idx + 1 < ast.myGenericList.mySeq.len():
          result.add ", "
      result.add "]"
  of astVarEtcDeclMost:
    result.add ast.myVarEtcDeclMost.ident.myToRepr() & ": "
    result.add ast.myVarEtcDeclMost.type.myToRepr()

proc constEval*(
  ast: AstNode,
  inputFname: string,
): int64 =
  proc myEval(
    ast: AstNode
  ): int64 =
    result = 0i64
    proc doError() =
      doAssert(
        false,
        (
          "Invalid constant expression ("
        ) & (
          ast.lexMain.locMsg(inputFname=inputFname)
        ) & (
          ")"
        )
      )
    case ast.kind:
      of astU64Lit:
        result = ast.myU64Lit.u64Val.int64()
      of astBinop:
        case ast.myBinop.kind:
        of binopCmpEq:
          if ast.myBinop.left.myEval() == ast.myBinop.right.myEval():
            result = 1i64
          else:
            result = 0i64
        of binopCmpNe:
          if ast.myBinop.left.myEval() != ast.myBinop.right.myEval():
            result = 1i64
          else:
            result = 0i64
        of binopCmpLt:
          if ast.myBinop.left.myEval() < ast.myBinop.right.myEval():
            result = 1i64
          else:
            result = 0i64
        of binopCmpGt:
          if ast.myBinop.left.myEval() > ast.myBinop.right.myEval():
            result = 1i64
          else:
            result = 0i64
        of binopCmpLe:
          if ast.myBinop.left.myEval() <= ast.myBinop.right.myEval():
            result = 1i64
          else:
            result = 0i64
        of binopCmpGe:
          if ast.myBinop.left.myEval() >= ast.myBinop.right.myEval():
            result = 1i64
          else:
            result = 0i64
        #--------
        of binopPlus:
          result = (
            ast.myBinop.left.myEval() + ast.myBinop.right.myEval()
          )
        of binopMinus:
          result = (
            ast.myBinop.left.myEval() - ast.myBinop.right.myEval()
          )
        of binopMul:
          result = (
            ast.myBinop.left.myEval() * ast.myBinop.right.myEval()
          )
        of binopDiv:
          result = (
            ast.myBinop.left.myEval() div ast.myBinop.right.myEval()
          )
        of binopMod:
          result = (
            ast.myBinop.left.myEval() mod ast.myBinop.right.myEval()
          )
        of binopBitAnd:
          result = (
            ast.myBinop.left.myEval() and ast.myBinop.right.myEval()
          )
        of binopBitOr:
          result = (
            ast.myBinop.left.myEval() or ast.myBinop.right.myEval()
          )
        of binopBitXor:
          result = (
            ast.myBinop.left.myEval() xor ast.myBinop.right.myEval()
          )
        of binopLogicAnd:
          if (
            (
              ast.myBinop.left.myEval() != 0i64
            ) and (
              ast.myBinop.right.myEval() != 0i64
            )
          ):
            result = 1i64
          else:
            result = 0i64
        of binopLogicOr:
          if (
            (
              ast.myBinop.left.myEval() != 0i64
            ) or (
              ast.myBinop.right.myEval() != 0i64
            )
          ):
            result = 1i64
          else:
            result = 0i64
        of binopBitShl:
          result = (
            ast.myBinop.left.myEval() shl ast.myBinop.right.myEval()
          )
        of binopBitShr:
          result = (
            ast.myBinop.left.myEval() shr ast.myBinop.right.myEval()
          )
        else:
          doAssert(
            false,
            "eek!"
          )
      of astUnop:
        case ast.myUnop.kind:
        of unopPlus:
          result = ast.myUnop.obj.myEval()
        of unopMinus:
          result = (-ast.myUnop.obj.myEval())
        of unopLogicNot:
          if (ast.myUnop.obj.myEval() != 0i64):
            result = 1i64
          else:
            result = 0i64
        of unopBitInvert:
          result = not ast.myUnop.obj.myEval()
        else:
          doError()
      else:
        doError()
  result = ast.myEval()
