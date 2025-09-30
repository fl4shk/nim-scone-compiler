import std/options
import std/macros
import std/strutils

import dataStructuresMisc
#import symTbl
import typeInfo

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

macro mkEnumAstStmtKind(): untyped =
  var tempSeq: seq[NimNode] = @[]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if not h[2]:
      continue
    if h[3] == metaAstStmt:
      tempSeq.add ident("stmt" & h[0])
  result = newEnum(
    name=ident("AstStmtKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )
  echo result.repr()
mkEnumAstStmtKind()
macro mkEnumAstExprKind(): untyped =
  var tempSeq: seq[NimNode] = @[]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if not h[2]:
      continue
    if h[3] == metaAstExpr:
      tempSeq.add ident("expr" & h[0])
  result = newEnum(
    name=ident("AstExprKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )
  echo result.repr()
mkEnumAstExprKind()
macro mkEnumAstTypeSubKind(): untyped =
  var tempSeq: seq[NimNode] = @[]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if not h[2]:
      continue
    if h[3] == metaAstTypeSub:
      tempSeq.add ident("typeSub" & h[0])
  result = newEnum(
    name=ident("AstTypeSubKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )
  echo result.repr()
mkEnumAstTypeSubKind()

#dumpTree:
#  type
#    Asdf* = ref AsdfObj
#    AsdfObj* = object
#StmtList
#  TypeSection
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "Asdf"
#      Empty
#      RefTy
#        Ident "AsdfObj"
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "AsdfObj"
#      Empty
#      ObjectTy
#        Empty
#        Empty
#        Empty

macro mkAstHierMost(): untyped =
  result = newNimNode(nnkTypeSection)
  #var otherSeq: seq[HelperTokKind]
  #var stmtIdxSeq: seq[int] = @[]
  #var exprIdxSeq: seq[int] = @[]
  #var typeSubIdxSeq: seq[int] = @[]
  #var idxPtrSeq: seq[seq[int]] = @[]
  var myIdxSeqArr: array[metaAstInfoArr.len(), seq[int]]

  template recList(): untyped = objTy[2]
  template recCase(): untyped = recList[1]
  template addFieldLexMain(): untyped =
    var identDefs = newNimNode(nnkIdentDefs)
    identDefs.add(
      mkPubIdent("lexMain"),
      ident("LexMain"),
      newNimNode(nnkEmpty),
    )
    recList.add identDefs
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if not h[2]:
      continue

    #case h[3]:
    #of metaAstNone:
    #  #otherSeq.add h
    #  discard
    #of metaAstStmt:
    #  stmtIdxSeq.add idx
    #of metaAstExpr:
    #  exprIdxSeq.add idx
    #of metaAstTypeSub:
    #  typeSubIdxSeq.add idx
    let info = metaAstInfoArr[int(h[3])]
    if info.kind.isSome:
      myIdxSeqArr[int(h[3])].add idx

    var tempStr: string = ""
    case h[3]:
    of metaAstNone:
      tempStr = "Ast" & h[0]
    else:
      tempStr = "SubAst" & h[0]

    var typeDefRef = newNimNode(nnkTypeDef)
    typeDefRef.add mkPubIdent(tempStr)
    typeDefRef.add newNimNode(nnkEmpty)
    typeDefRef.add newNimNode(nnkRefTy)
    typeDefRef[2].add ident(tempStr & "Obj")
    result.add typeDefRef

    var typeDef = newNimNode(nnkTypeDef)
    typeDef.add mkPubIdent(tempStr & "Obj")

    typeDef.add newNimNode(nnkEmpty)
    var objTy = add(
      newNimNode(nnkObjectTy),
      [
        newNimNode(nnkEmpty),
        newNimNode(nnkEmpty),
      ],
    )
    #template recList(): untyped = objTy[2]
    #var didAddRecList: bool = false
    objTy.add(newNimNode(nnkRecList))
    addFieldLexMain()
    for jdx in 0 ..< h[4].len():
      #if jdx == 0:
      #  didAddRecList = true
      #  objTy.add(newNimNode(nnkRecList))
      #  addFieldLexMain()
      let field = h[4][jdx]
      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add mkPubIdent(field[0])

      var specAstStr: string = ""
      #if field[2].len() == 1:
      specAstStr = "Ast" & field[2]
      #else:
      #  specAstStr = "AstNode"

      case field[1]:
      of astValAstNode:
        identDefs.add ident(specAstStr)
      of astValSeqAstNode:
        identDefs.add(
          add(
            newNimNode(nnkBracketExpr),
            ident("seq"),
            ident(specAstStr),
          )
        )
      of astValOptAstNode:
        identDefs.add(
          add(newNimNode(nnkBracketExpr),
            ident("Option"),
            ident(specAstStr),
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
    #if not didAddRecList:
    #  #objTy.add newNimNode(nnkEmpty)
    #  addFieldLexMain()
    typeDef.add objTy
    result.add typeDef

  for infoIdx in 0 ..< metaAstInfoArr.len():
    let info = metaAstInfoArr[infoIdx]
    if not info.kind.isSome:
      continue

    block:
      var typeDefRef = newNimNode(nnkTypeDef)
      typeDefRef.add mkPubIdent("Ast" & info.nameUpper)
      typeDefRef.add newNimNode(nnkEmpty)
      typeDefRef.add newNimNode(nnkRefTy)
      typeDefRef[2].add ident("Ast" & info.nameUpper & "Obj")
      result.add typeDefRef

    block:
      var typeDef = newNimNode(nnkTypeDef)
      typeDef.add mkPubIdent("Ast" & info.nameUpper & "Obj")
      typeDef.add newNimNode(nnkEmpty)

      var objTy = add(
        newNimNode(nnkObjectTy),
        [
          newNimNode(nnkEmpty),
          newNimNode(nnkEmpty),
          newNimNode(nnkRecList),
        ]
      )
      addFieldLexMain()
      recList.add newNimNode(nnkRecCase)

      block:
        var identDefs = newNimNode(nnkIdentDefs)
        identDefs.add mkPubIdent("kind")
        identDefs.add ident("Ast" & info.nameUpper & "Kind")
        identDefs.add newNimNode(nnkEmpty)
        recCase.add identDefs

      for idx in myIdxSeqArr[infoIdx]:
        let h = helperTokKindSeq[idx]
        let tempStr = "SubAst" & h[0]

        var ofBranch = newNimNode(nnkOfBranch)
        ofBranch.add ident(info.nameLower & h[0])
        var identDefs = newNimNode(nnkIdentDefs)
        identDefs.add mkPubIdent("my" & h[0])
        identDefs.add ident(tempStr)
        identDefs.add newNimNode(nnkEmpty)
        ofBranch.add identDefs
        recCase.add ofBranch
      typeDef.add objTy
      result.add typeDef

  #block:
  #  var typeDefRef = newNimNode(nnkTypeDef)
  #  typeDefRef.add mkPubIdent("AstExpr")
  #  typeDefRef.add newNimNode(nnkEmpty)
  #  typeDefRef.add newNimNode(nnkRefTy)
  #  typeDefRef[2].add ident("AstExprObj")
  #  result.add typeDefRef

  #block:
  #  var typeDef = newNimNode(nnkTypeDef)
  #  typeDef.add mkPubIdent("AstExprObj")
  #  typeDef.add newNimNode(nnkEmpty)

  #  var objTy = add(
  #    newNimNode(nnkObjectTy),
  #    [
  #      newNimNode(nnkEmpty),
  #      newNimNode(nnkEmpty),
  #      newNimNode(nnkRecList),
  #    ]
  #  )
  #  addFieldLexMain()
  #  recList.add newNimNode(nnkRecCase)

  #  block:
  #    var identDefs = newNimNode(nnkIdentDefs)
  #    var bracketExpr = newNimNode(nnkBracketExpr)
  #    bracketExpr.add(
  #      ident("Option"),
  #      ident("TypeInfo"),
  #    )
  #    identDefs.add(
  #      mkPubIdent("typeInfo"),
  #      bracketExpr,
  #      newNimNode(nnkEmpty),
  #    )
  #    recList.add identDefs

  #  block:
  #    var identDefs = newNimNode(nnkIdentDefs)
  #    identDefs.add mkPubIdent("kind")
  #    identDefs.add ident("AstExprKind")
  #    identDefs.add newNimNode(nnkEmpty)
  #    recCase.add identDefs

  #  for idx in exprIdxSeq:
  #    let h = helperTokKindSeq[idx]
  #    let tempStr = "SubAst" & h[0]

  #    var ofBranch = newNimNode(nnkOfBranch)
  #    ofBranch.add ident("expr" & h[0])
  #    var identDefs = newNimNode(nnkIdentDefs)
  #    identDefs.add mkPubIdent("my" & h[0])
  #    identDefs.add ident(tempStr)
  #    identDefs.add newNimNode(nnkEmpty)
  #    ofBranch.add identDefs
  #    recCase.add ofBranch
  #  typeDef.add objTy
  #  result.add typeDef

  #block:
  #  var typeDefRef = newNimNode(nnkTypeDef)
  #  typeDefRef.add mkPubIdent("AstTypeSub")
  #  typeDefRef.add newNimNode(nnkEmpty)
  #  typeDefRef.add newNimNode(nnkRefTy)
  #  typeDefRef[2].add ident("AstTypeSubObj")
  #  result.add typeDefRef

  #block:
  #  var typeDef = newNimNode(nnkTypeDef)
  #  typeDef.add mkPubIdent("AstTypeSubObj")
  #  typeDef.add newNimNode(nnkEmpty)

  #  var objTy = add(
  #    newNimNode(nnkObjectTy),
  #    [
  #      newNimNode(nnkEmpty),
  #      newNimNode(nnkEmpty),
  #      newNimNode(nnkRecList),
  #    ]
  #  )
  #  addFieldLexMain()
  #  recList.add newNimNode(nnkRecCase)

  #  block:
  #    var identDefs = newNimNode(nnkIdentDefs)
  #    identDefs.add mkPubIdent("kind")
  #    identDefs.add ident("AstTypeSubKind")
  #    identDefs.add newNimNode(nnkEmpty)
  #    recCase.add identDefs

  #  for idx in typeSubIdxSeq:
  #    let h = helperTokKindSeq[idx]
  #    let tempStr = "SubAst" & h[0]

  #    var ofBranch = newNimNode(nnkOfBranch)
  #    ofBranch.add ident("typeSub" & h[0])
  #    var identDefs = newNimNode(nnkIdentDefs)
  #    identDefs.add mkPubIdent("my" & h[0])
  #    identDefs.add ident(tempStr)
  #    identDefs.add newNimNode(nnkEmpty)
  #    ofBranch.add identDefs
  #    recCase.add ofBranch
  #  typeDef.add objTy
  #  result.add typeDef

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
    #block:
    #  var identDefs = newNimNode(nnkIdentDefs)
    #  identDefs.add(
    #    mkPubIdent("lexMain"),
    #    ident("LexMain"),
    #    newNimNode(nnkEmpty),
    #  )
    #  recList.add identDefs
    #block:
    #  var identDefs = newNimNode(nnkIdentDefs)
    #  var bracketExpr = newNimNode(nnkBracketExpr)
    #  bracketExpr.add(
    #    ident("Option"),
    #    ident("TypeInfo"),
    #  )
    #  identDefs.add(
    #    mkPubIdent("typeInfo"),
    #    #ident("int"),
    #    bracketExpr,
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

      proc innerProc(
        name: string
      ) =
        var ofBranch = newNimNode(nnkOfBranch)
        ofBranch.add(
          ident("ast" & name)
        )
        var obIdentDefs = newNimNode(nnkIdentDefs)
        obIdentDefs.add(
          mkPubIdent("my" & name),
          ident("Ast" & name),
          newNimNode(nnkEmpty)
        )

        ofBranch.add obIdentDefs
        recCase.add ofBranch
      for idx in 0 ..< helperTokKindSeq.len():
        let h = helperTokKindSeq[idx]
        if not h[2]:
          continue
        if h[3] != metaAstNone:
          continue
        innerProc(h[0])
      for infoIdx in 0 ..< metaAstInfoArr.len():
        let info = metaAstInfoArr[infoIdx]
        if info.kind.isSome:
          innerProc(info.nameUpper)
      #innerProc("Stmt")
      #innerProc("Expr")
      #innerProc("TypeSub")

      recList.add recCase

    #objTy.add 

    typeDef.add objTy
    result.add typeDef

    #echo result.repr()


  #block:
  #  for idx in exprIdxSeq:
  #    let h = helperTokKindSeq[idx]
  echo result.repr()

mkAstHierMost()

#StmtList
#  ProcDef
#    Postfix
#      Ident "*"
#      Ident "toAstNode"
#    Empty
#    Empty
#    FormalParams
#      Ident "AstNode"
#      IdentDefs
#        Ident "obj"
#        Ident "AstModule"
#        Empty
#    Empty
#    Empty
#    StmtList
#      Asgn
#        Ident "result"
#        ObjConstr
#          Ident "AstNode"
#          ExprColonExpr
#            Ident "kind"
#            Ident "astModule"
#          ExprColonExpr
#            Ident "myModule"
#            Ident "obj"
#dumpTree:
#  proc toAstNode*(
#    obj: AstModule,
#  ): AstNode =
#    result = AstNode(
#      kind: astModule,
#      myModule: obj,
#    )

macro mkToAstNodeProcs(): untyped =
  var tempSeq: seq[string]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if h[2] and h[3] == metaAstNone:
      tempSeq.add helperTokKindSeq[idx][0]
  for infoIdx in 0 ..< metaAstInfoArr.len():
    let info = metaAstInfoArr[infoIdx]
    if info.kind.isSome:
      tempSeq.add info.nameUpper
  #tempSeq.add "Stmt"
  #tempSeq.add "Expr"
  #tempSeq.add "TypeSub"

  result = newNimNode(nnkStmtList)
  for idx in 0 ..< tempSeq.len():
    let identKind = ident("ast" & tempSeq[idx])
    let identObjType = ident("Ast" & tempSeq[idx])
    let identArgName = ident("my" & tempSeq[idx])
    let identObj = ident("obj")

    result.add quote do:
      proc toAstNode*(
        `identObj`: `identObjType`
      ): AstNode =
        result = AstNode(
          kind: `identKind`,
          `identArgName`: `identObj`,
        )
  echo result.repr()

mkToAstNodeProcs()

#macro mkAstHier(): untyped =
#  #result = newTypeSection(
#  #)
#  #result = quote do:
#  #  discard
#  result = newNimNode(nnkTypeSection)
#  #var typeDefSeq: seq[NimNode]
#  for idx in 0 ..< helperTokKindSeq.len():
#    let h = helperTokKindSeq[idx]
#    if not h[2]:
#      continue
#
#    var typeDef = newNimNode(nnkTypeDef)
#
#    typeDef.add mkPubIdent("Ast" & h[0])
#    typeDef.add newNimNode(nnkEmpty)
#    #var objTyArgs: seq[NimNode]
#    #objTyArgs
#
#    var objTy = add(
#      newNimNode(nnkObjectTy),
#      [
#        newNimNode(nnkEmpty),
#        newNimNode(nnkEmpty),
#        #newNimNode(nnkRecList),
#      ]
#    )
#    #objTy.dumpTree()
#
#    #for arg in objTyArgs:
#    #  objTy.add arg
#    template recList(): untyped = objTy[2]
#    var didAddRecList: bool = false
#    for jdx in 0 ..< h[3].len():
#      if jdx == 0:
#        didAddRecList = true
#        objTy.add(newNimNode(nnkRecList))
#      let field = h[3][jdx]
#      #echo "jdx:" & $jdx & " " & $h
#      #echo "field:" & $field
#
#      var identDefs = newNimNode(nnkIdentDefs)
#      identDefs.add mkPubIdent(field[0])
#      #echo identDefs.repr()
#
#      case field[1]:
#      of astValAstNode:
#        identDefs.add ident("AstNode")
#      of astValSeqAstNode:
#        identDefs.add(
#          add(
#            newNimNode(nnkBracketExpr),
#            ident("seq"),
#            ident("AstNode"),
#          )
#        )
#      of astValOptAstNode:
#        identDefs.add(
#          add(newNimNode(nnkBracketExpr),
#            ident("Option"),
#            ident("AstNode"),
#          )
#        )
#      of astValString:
#        identDefs.add ident("string")
#      of astValU64:
#        identDefs.add ident("uint64")
#      of astValBool:
#        identDefs.add ident("bool")
#      of astValUnopKind:
#        identDefs.add ident("AstUnopKind")
#      of astValBinopKind:
#        identDefs.add ident("AstBinopKind")
#      of astValAssignEtcKind:
#        identDefs.add ident("AstAssignEtcKind")
#      of astValBasicTypeKind:
#        identDefs.add ident("AstBasicTypeKind")
#
#      identDefs.add newNimNode(nnkEmpty)
#      recList.add identDefs
#    if not didAddRecList:
#      objTy.add newNimNode(nnkEmpty)
#    typeDef.add objTy
#
#    result.add typeDef
#
#  block:
#    var typeDef = newNimNode(nnkTypeDef)
#    typeDef.add mkPubIdent("AstNode")
#    typeDef.add newNimNode(nnkEmpty)
#    typeDef.add(
#      add(newNimNode(nnkRefTy), ident("AstNodeObj"))
#    )
#    result.add typeDef
#    #echo result.repr()
#
#  block:
#    var typeDef = newNimNode(nnkTypeDef)
#    typeDef.add mkPubIdent("AstNodeObj")
#    typeDef.add newNimNode(nnkEmpty)
#    var objTy = newNimNode(nnkObjectTy)
#    template recList(): untyped = objTy[2]
#    objTy.add(
#      newNimNode(nnkEmpty), newNimNode(nnkEmpty), newNimNode(nnkRecList)
#    )
#    block:
#      var identDefs = newNimNode(nnkIdentDefs)
#      identDefs.add(
#        mkPubIdent("lexMain"),
#        ident("LexMain"),
#        newNimNode(nnkEmpty),
#      )
#      recList.add identDefs
#    block:
#      #IdentDefs
#      #  Postfix
#      #    Ident "*"
#      #    Ident "symTblId"
#      #  BracketExpr
#      #    Ident "Option"
#      #    Ident "int"
#      #  Empty
#
#      var identDefs = newNimNode(nnkIdentDefs)
#      var bracketExpr = newNimNode(nnkBracketExpr)
#      bracketExpr.add(
#        ident("Option"),
#        #ident("int"),
#        ident("TypeInfo"),
#      )
#      identDefs.add(
#        mkPubIdent("typeInfo"),
#        #ident("int"),
#        bracketExpr,
#        newNimNode(nnkEmpty),
#      )
#      recList.add identDefs
#      #echo recList.repr()
#      #dumpTree:
#      #  type 
#      #    Aaaa = object
#      #      symTblId*: Option[int]
#      #let tempNode = quote do:
#      #  symTblId*: Option[int]
#      #  
#      #recList.add tempNode
#    #block:
#    #  var identDefs = newNimNode(nnkIdentDefs)
#    #  identDefs.add(
#    #    mkPubIdent("parentExpr"),
#    #    ident("AstNode"),
#    #    newNimNode(nnkEmpty),
#    #  )
#    #  recList.add identDefs
#    block:
#      var recCase = newNimNode(nnkRecCase)
#      var identDefs = newNimNode(nnkIdentDefs)
#      identDefs.add(
#        mkPubIdent("kind"),
#        ident("AstKind"),
#        newNimNode(nnkEmpty),
#      )
#      recCase.add identDefs
#      for idx in 0 ..< helperTokKindSeq.len():
#        let h = helperTokKindSeq[idx]
#        if not h[2]:
#          continue
#
#        var ofBranch = newNimNode(nnkOfBranch)
#        ofBranch.add(
#          ident("ast" & h[0])
#        )
#        var obIdentDefs = newNimNode(nnkIdentDefs)
#        obIdentDefs.add(
#          mkPubIdent("my" & h[0]),
#          ident("Ast" & h[0]),
#          newNimNode(nnkEmpty)
#        )
#
#        ofBranch.add obIdentDefs
#        recCase.add ofBranch
#      recList.add recCase
#
#    #objTy.add 
#
#    typeDef.add objTy
#    result.add typeDef
#
#    #echo result.repr()
#
#  #for idx in 0 ..< helperTokKindSeq.len():
#  #  let h = helperTokKindSeq[idx]
#  #  if not h[2]:
#  #    continue
#
#
#mkAstHier()

proc doIndent*(
  indent: uint,
): string =
  for idx in 0 ..< indent:
    result.add " "

##macro toStr*(
##  ast: AstNode
##): untyped =
##  #dumpTree(ast)
##  case ast.kind:
##  of 
##  discard
#

proc toStr*(
  astSeq: seq[AstNode],
  indent: uint,
): string

proc toStr*(
  ast: AstNode,
  indent: uint,
): string

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
  var tempSeq: seq[AstNode]
  proc innerProc(
    idx: int,
    postFirstLoop: bool,
  ): NimNode =
    let h = helperTokKindSeq[idx]
    let info = metaAstInfoArr[int(h[3])]

    if not h[2] or info.kind.isSome:
      if not postFirstLoop:
        #if info.kind.isSome:
        #  myIdxSeqArr[int(h[3])].add idx
        #continue
        return

    var ofBranch = newNimNode(nnkOfBranch)
    ofBranch.add ident("ast" & h[0])
    var stmtList = newNimNode(nnkStmtList)

    var myCmd0 = newNimNode(nnkCommand)
    let tempAstName = "(Ast" & h[0]
    let tempNimNode = quote do:
      `tempAstName`
      
    myCmd0.add(
      add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
      tempNimNode
    )
    stmtList.add myCmd0

    var tempNl: NimNode
    var tempI: NimNode

    for jdx in 0 ..< h[4].len():
      let field = h[4][jdx]
      if jdx == 0 and h[4].len > 1:
        stmtList.add quote do:
          result.add "\n"
      if h[4].len <= 1:
        stmtList.add quote do:
          x = indent
      if h[4].len() > 1:
        tempNl = quote do:
          "\n"
        tempI = quote do:
          i
      else:
        tempNl = quote do:
          ""
        tempI = quote do:
          " "
      var toAdd: NimNode = nil

      let myAstIdent = ident("ast")
      let myMbrIdent = ident("my" & h[0])
      let mbrStr = newLit(field[0])
      let myInnerMbrIdent = ident(field[0])
      #let myToStrIdent = ident("toStr")
      let myDualDotExpr = quote do:
        `myAstIdent`.`myMbrIdent`.`myInnerMbrIdent`
      let myTempSeqIdent = ident("tempSeq")
      let myKdxIdent = ident("kdx")

      case field[1]:
      of astValAstNode:
        toAdd = quote do:
          result.add(
            (
              #"\n" & i & 
              #`tempNl` & 
              `tempI` & `mbrStr` & " "
            ) & (
              `myDualDotExpr`.toAstNode().toStr(x) #& i & ")\n"
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
            )
          )
          `myTempSeqIdent`.setLen(0)
          for `myKdxIdent` in 0 ..< `myDualDotExpr`.len():
            `myTempSeqIdent`.add(
              `myDualDotExpr`[`myKdxIdent`].toAstNode()
            )
            #& (
            #  #`myDualDotExpr`.toAstNode().toStr(x) #& "\n"
            #)
          result.add `myTempSeqIdent`.toStr(x)
          result.add(
            `tempNl`
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
                `myDualDotExpr`.get.toAstNode().toStr(x)
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
        h[4].len() == 0,
        h[4].len() == 1,
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
    #result.add ofBranch
    result = ofBranch

  macro doCaseStmt(): untyped =
    result = newNimNode(nnkCaseStmt)
    result.add(
      add(newNimNode(nnkDotExpr), ident("ast"), ident("kind"))
    )
    #echo result.repr()
    var myIdxSeqArr: array[metaAstInfoArr.len(), seq[int]]

    for idx in 0 ..< helperTokKindSeq.len():
      let h = helperTokKindSeq[idx]
      let info = metaAstInfoArr[int(h[3])]

      if not h[2] or info.kind.isSome:
        #if not postFirstLoop:
        if info.kind.isSome:
          myIdxSeqArr[int(h[3])].add idx
        continue
      result.add innerProc(idx=idx, postFirstLoop=false)
    #dumpTree result
    #echo "<-- -->"
    for infoIdx in 0 ..< metaAstInfoArr.len():
      let info = metaAstInfoArr[infoIdx]
      if not info.kind.isSome:
        continue
      var ofBranch = newNimNode(nnkOfBranch)
      ofBranch.add ident("ast" & info.nameUpper)
      var stmtList = newNimNode(nnkStmtList)
      var caseStmt = newNimNode(nnkCaseStmt)
      let myAst = ident("ast")
      let mySubAstObj = ident("my" & info.nameUpper)
      let mySubAstKind = ident("kind")
      let mySubAst: NimNode = quote do:
        `myAst`.`mySubAstObj`.`mySubAstKind`
      caseStmt.add(
        #add(
        #  newNimNode(nnkDotExpr),
        #  ident("ast")
        #)
        mySubAst
      )
      #var subElseBranch = newNimNode(nnkElse)
      #subStmtList.dumpTree

      for idx in myIdxSeqArr[infoIdx]:
        let h = helperTokKindSeq[idx]

        var subOfBranch = newNimNode(nnkOfBranch)
        subOfBranch.add ident(info.nameLower & h[0])
        var subStmtList = newNimNode(nnkStmtList)

        let mySubAstSubObj = ident("my" & h[0])
        let tempNimNode = quote do:
          `myAst`.`mySubAstObj`.`mySubAstSubObj`.toAstNode().toStr(x)

        var myCmd0 = newNimNode(nnkCommand)
        myCmd0.add(
          add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
          tempNimNode
        )
        subStmtList.add myCmd0
        subOfBranch.add subStmtList
        caseStmt.add subOfBranch

      stmtList.add caseStmt
      ofBranch.add stmtList
      result.add ofBranch

    echo result.repr()
  doCaseStmt()

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

##proc repr*(
##  ast: AstNode,
##  indent: uint,
##): string =
##  discard
#  
#proc `$`*(
#  ast: AstNode
#): string =
#  result = ast.toStr(0)
#
#proc toRepr*(
#  ast: AstNode,
#  parent: AstNode=nil,
#  indent: int=(-2),
#  #level: int=0,
#): string =
#  proc myToRepr(
#    otherAst: AstNode,
#    indent: int=(-2)
#  ): string =
#    result = otherAst.toRepr(parent=ast, indent=indent)
#
#  if ast == nil:
#    return "(eek! `nil`)"
#  var x = indent + 2
#  template i(): untyped =
#    doIndent(indent=uint(x))
#  var iPrev: Option[string] = none(string)
#  if indent >= 0:
#    iPrev = some(doIndent(indent=uint(indent)))
#
#  proc helperStmtSeq(
#    stmtSeq: seq[AstNode],
#    toSub: int=0,
#    #includeSemicolon: bool=true,
#  ): string =
#    for idx in 0 ..< stmtSeq.len():
#      result.add stmtSeq[idx].myToRepr(x - toSub) & "\n"
#      #if includeSemicolon:
#      #  result.add ";"
#      #result.add "\n"
#    
#
#  case ast.kind:
#  of astSrcFile:
#    result.add ast.mySrcFile.module.myToRepr() & ";\n\n"
#    for idx in 0 ..< ast.mySrcFile.funcDeclSeq.len():
#      result.add ast.mySrcFile.funcDeclSeq[idx].myToRepr() & ";\n\n"
#    for idx in 0 ..< ast.mySrcFile.structDeclSeq.len():
#      result.add ast.mySrcFile.structDeclSeq[idx].myToRepr() & ";\n\n"
#  of astIdent:
#    result.add ast.myIdent.strVal
#  of astU64Lit:
#    result.add $ast.myU64Lit.u64Val
#  of astStrLit:
#    result.add "\"" & ast.myStrLit.strLitVal & "\""
#  of astOpenarrLit:
#    result.add "$(" 
#    for idx in 0 ..< ast.myOpenarrLit.openarrLitSeq.len():
#      result.add ast.myOpenarrLit.openarrLitSeq[idx].myToRepr()
#      if idx + 1 < ast.myOpenarrLit.openarrLitSeq.len():
#        result.add ", "
#    result.add ")"
#  of astTrue:
#    result.add "true"
#  of astFalse:
#    result.add "false"
#  #of astPtr:
#  #  result.add "ptr"
#  #of astAddr:
#  #  result.add "(" & "addr " & ast.myAddr.obj.myToRepr(x) & ")"
#  of astDeref:
#    #result.add "(" & ast.myDeref.obj.myToRepr(x) & "@)"
#    result.add ast.myDeref.obj.myToRepr(x) & "@"
#  of astDot:
#    #result.add "("
#    result.add ast.myDot.left.myToRepr(x) & "."
#    result.add ast.myDot.right.myToRepr(x)
#    #result.add ")"
#  of astVar:
#    result.add i & "var " & ast.myVar.child.myToRepr()
#    if ast.myVar.optExpr.isSome:
#      result.add " = " & ast.myVar.optExpr.get().myToRepr()
#    result.add ";"
#  of astConst:
#    result.add i & "const " & ast.myConst.child.myToRepr()
#    result.add " = " & ast.myConst.expr.myToRepr()
#    result.add ";"
#  of astDef:
#    result.add "def " & ast.myDef.ident.myToRepr()
#    if ast.myDef.genericDecl.isSome:
#      result.add ast.myDef.genericDecl.get().myToRepr()
#    result.add "("
#    #result.add ast.myDef.argDeclSeq
#    for idx in 0 ..< ast.myDef.argDeclSeq.len():
#      result.add ast.myDef.argDeclSeq[idx].myToRepr()
#      if idx + 1 < ast.myDef.argDeclSeq.len():
#        result.add ", "
#    result.add ") -> " & ast.myDef.returnType.myToRepr() & " {\n"
#    #for idx in 0 ..< ast.myDef.stmtSeq.len():
#    #  result.add i & ast.myDef.stmtSeq[idx].myToRepr(x) & ";\n"
#    result.add helperStmtSeq(ast.myDef.stmtSeq)
#    result.add "}"
#  of astModule:
#    result.add "module " & ast.myModule.ident.myToRepr()
#  of astStruct:
#    result.add "struct " & ast.myStruct.ident.myToRepr()
#    if ast.myStruct.genericDecl.isSome:
#      result.add ast.myStruct.genericDecl.get().myToRepr() & " {\n"
#    for idx in 0 ..< ast.myStruct.fieldSeq.len():
#      result.add(
#        (
#          doIndent(uint(x + 2))
#        ) & (
#          ast.myStruct.fieldSeq[idx].myToRepr() & ";\n"
#        )
#      )
#    result.add "}"
#  of astEnum:
#    discard
#  of astVariant:
#    discard
#  of astExtern:
#    discard
#  of astCextern:
#    discard
#  of astImport:
#    discard
#  of astCImport:
#    discard
#  of astScope:
#    result.add iPrev.get() & "scope {\n"
#    result.add helperStmtSeq(ast.myScope.stmtSeq)
#    result.add iPrev.get() & "}"
#  of astIf:
#    result.add (
#      i & "if " & ast.myIf.expr.myToRepr() & " {\n"
#    )
#    result.add helperStmtSeq(ast.myIf.stmtSeq)
#    result.add i & "}"
#    #if ast.myIf.elifSeq.len() > 0:
#    #  result.add ast.myIf.elifSeq[0].myToRepr(x)
#    #  #if ast.myIf.optElse.isSome:
#    #  #  result.add " " & ast.myIf.optElse.get().myToRepr(x)
#    #for myElif in ast.myIf.elifSeq:
#    for idx in 0 ..< ast.myIf.elifSeq.len():
#      result.add " " & ast.myIf.elifSeq[idx].myToRepr(x - 2)
#
#    if ast.myIf.optElse.isSome:
#      result.add " " & ast.myIf.optElse.get().myToRepr(x - 2)
#  of astElif:
#    result.add "elif " & ast.myElif.expr.myToRepr() & " {\n"
#    result.add helperStmtSeq(ast.myElif.stmtSeq)
#    result.add i & "}"
#    discard
#  of astElse:
#    result.add "else {\n"
#    result.add helperStmtSeq(ast.myElse.stmtSeq)
#    result.add i & "}"
#  of astSwitch:
#    result.add i & "switch " & ast.mySwitch.expr.myToRepr() & " {\n"
#    result.add helperStmtSeq(ast.mySwitch.caseSeq)
#    if ast.mySwitch.optDefault.isSome:
#      result.add i & ast.mySwitch.optDefault.get().myToRepr(x - 2)
#      result.add "\n"
#    result.add i & "}"
#  of astCase:
#    result.add iPrev.get() & "case " & ast.myCase.expr.myToRepr() & " {\n"
#    result.add helperStmtSeq(ast.myCase.stmtSeq)
#    result.add iPrev.get() & "}"
#  of astDefault:
#    result.add "default {\n"
#    result.add helperStmtSeq(ast.myDefault.stmtSeq)
#    result.add i & "}"
#  of astFor:
#    result.add i & "for "
#    result.add ast.myFor.ident.myToRepr() & " in "
#    result.add ast.myFor.exprPre.myToRepr()
#    if ast.myFor.isUntil:
#      result.add " until "
#    else:
#      result.add " to "
#    result.add ast.myFor.exprPost.myToRepr() & " {\n"
#    result.add helperStmtSeq(ast.myFor.stmtSeq)
#    result.add i & "}"
#  of astWhile:
#    result.add i & "while "
#    result.add ast.myWhile.expr.myToRepr() & " {\n"
#    result.add helperStmtSeq(ast.myWhile.stmtSeq)
#    result.add i & "}"
#  of astContinue:
#    result.add i & "continue" & ";"
#  of astBreak:
#    result.add i & "break" & ";"
#  of astReturn:
#    result.add i & "return"
#    if ast.myReturn.optExpr.isSome:
#      result.add " " & ast.myReturn.optExpr.get().myToRepr(x)
#    result.add ";"
#  of astArray:
#    result.add "array["
#    result.add ast.myArray.dim.myToRepr() & "; "
#    result.add ast.myArray.elemType.myToRepr()
#    result.add "]"
#  of astOpenarray:
#    result.add "openarray["
#    result.add ast.myOpenarray.elemType.myToRepr()
#    result.add "]"
#  of astBuiltinTypeCast:
#    result.add ast.myBuiltinTypeCast.type.myToRepr()
#    result.add "("
#    result.add ast.myBuiltinTypeCast.obj.myToRepr()
#    result.add ")"
#  of astUnop:
#    var inclParens: bool = false
#    let myUnopExprOp = (
#      AstExprOp(
#        kind: exprOpUnop,
#        myUnop: ast.myUnop.kind,
#      )
#    )
#    if parent != nil:
#      var myObjExprOp: AstExprOp
#      case parent.kind:
#      of astUnop:
#        #inclParens = false
#        #myObjExprOp.kind = exprOpUnop
#        #myObjExprOp.myUnop = parent.myUnop.kind
#        #inclParens = myUnopExprOp.cmpPrioLt(myObjExprOp)
#        inclParens = true
#      of astBinop:
#        #inclParens = false
#        myObjExprOp.kind = exprOpBinop
#        myObjExprOp.myBinop = parent.myBinop.kind
#        inclParens = myUnopExprOp.cmpPrioLt(myObjExprOp)
#      else:
#        inclParens = false
#        discard
#
#    if inclParens:
#      result.add "("
#    result.add(
#      helperTokKindSeq[uint(ast.myUnop.kind.unopToTok())][1].get()
#    )
#    if ast.myUnop.kind == unopAddr:
#      result.add " "
#    result.add ast.myUnop.obj.myToRepr()
#    if inclParens:
#      result.add ")"
#  of astBinop:
#    var inclParens: bool = false
#    let myBinopExprOp = (
#      AstExprOp(
#        kind: exprOpBinop,
#        myBinop: ast.myBinop.kind,
#      )
#    )
#    if parent != nil:
#      var myObjExprOp: AstExprOp
#      case parent.kind:
#      of astUnop:
#        #inclParens = false
#        myObjExprOp.kind = exprOpUnop
#        myObjExprOp.myUnop = parent.myUnop.kind
#        inclParens = myBinopExprOp.cmpPrioLt(myObjExprOp)
#      of astBinop:
#        #inclParens = false
#        myObjExprOp.kind = exprOpBinop
#        myObjExprOp.myBinop = parent.myBinop.kind
#        inclParens = myBinopExprOp.cmpPrioLt(myObjExprOp)
#      else:
#        inclParens = false
#        discard
#
#    if inclParens:
#      result.add "("
#    result.add ast.myBinop.left.myToRepr() & " "
#    result.add(
#      helperTokKindSeq[uint(ast.myBinop.kind.binopToTok())][1].get()
#    )
#    result.add " " & ast.myBinop.right.myToRepr()
#    if inclParens:
#      result.add ")"
#  of astAssignEtc:
#    #result.add "("
#    result.add i & ast.myAssignEtc.left.myToRepr() & " "
#    result.add(
#      helperTokKindSeq[
#        uint(ast.myAssignEtc.kind.assignEtcToTok())
#      ][1].get()
#    )
#    result.add " " & ast.myAssignEtc.right.myToRepr()
#    #result.add ")"
#    result.add ";"
#  of astBasicType:
#    case ast.myBasicType.kind:
#    of basicTypeVoid:
#      result.add "void"
#    of basicTypeBool:
#      result.add "bool"
#    of basicTypeU8:
#      result.add "u8"
#    of basicTypeI8:
#      result.add "i8"
#    of basicTypeU16:
#      result.add "u16"
#    of basicTypeI16:
#      result.add "i16"
#    of basicTypeU32:
#      result.add "u32"
#    of basicTypeI32:
#      result.add "i32"
#    of basicTypeU64:
#      result.add "u64"
#    of basicTypeI64:
#      result.add "i64"
#    of basicTypeF32:
#      result.add "f32"
#    of basicTypeF64:
#      result.add "f64"
#    of basicTypeChar:
#      result.add "char"
#    of basicTypeString:
#      result.add "string"
#  of astNamedType:
#    result.add ast.myNamedType.ident.myToRepr()
#    result.add ast.myNamedType.genericImpl.myToRepr()
#  of astType:
#    if ast.myType.kwVar:
#      doAssert(
#        ast.myType.ptrDim == 0,
#        "eek! " & $ast
#      )
#      result.add "var "
#    if ast.myType.ptrDim > 0:
#      doAssert(
#        not ast.myType.kwVar,
#        "eek! " & $ast
#      )
#      for idx in 0 ..< ast.myType.ptrDim:
#        result.add "ptr "
#    result.add ast.myType.child.myToRepr()
#  of astFuncCall:
#    result.add ast.myFuncCall.ident.myToRepr()
#    result.add ast.myFuncCall.genericImpl.myToRepr()
#    result.add "("
#    for idx in 0 ..< ast.myFuncCall.argImplSeq.len():
#      result.add ast.myFuncCall.argImplSeq[idx].myToRepr()
#      if idx + 1 < ast.myFuncCall.argImplSeq.len():
#        result.add ", "
#    result.add ")"
#  of astStmtExprLhs:
#    result.add i & ast.myStmtExprLhs.expr.myToRepr() & ";"
#  #of astStmtFuncCall:
#  #  result.add i & ast.myStmtFuncCall.funcCall.myToRepr() & ";"
#  of astFuncNamedArgImpl:
#    result.add ast.myFuncNamedArgImpl.ident.myToRepr() & "="
#    result.add ast.myFuncNamedArgImpl.expr.myToRepr()
#  of astGenericNamedArgImpl:
#    result.add ast.myGenericNamedArgImpl.ident.myToRepr() & "="
#    result.add ast.myGenericNamedArgImpl.type.myToRepr()
#  of astGenericList:
#    if ast.myGenericList.mySeq.len() > 0:
#      result.add "["
#      for idx in 0 ..< ast.myGenericList.mySeq.len():
#        result.add ast.myGenericList.mySeq[idx].myToRepr()
#        if idx + 1 < ast.myGenericList.mySeq.len():
#          result.add ", "
#      result.add "]"
#  of astVarEtcDeclMost:
#    result.add ast.myVarEtcDeclMost.ident.myToRepr() & ": "
#    result.add ast.myVarEtcDeclMost.type.myToRepr()
#
#proc constEval*(
#  ast: AstNode,
#  inputFname: string,
#): int64 =
#  proc myEval(
#    ast: AstNode
#  ): int64 =
#    result = 0i64
#    proc doError() =
#      doAssert(
#        false,
#        (
#          "Invalid constant expression ("
#        ) & (
#          ast.lexMain.locMsg(inputFname=inputFname)
#        ) & (
#          ")"
#        )
#      )
#    case ast.kind:
#      of astU64Lit:
#        result = ast.myU64Lit.u64Val.int64()
#      of astBinop:
#        case ast.myBinop.kind:
#        of binopCmpEq:
#          if ast.myBinop.left.myEval() == ast.myBinop.right.myEval():
#            result = 1i64
#          else:
#            result = 0i64
#        of binopCmpNe:
#          if ast.myBinop.left.myEval() != ast.myBinop.right.myEval():
#            result = 1i64
#          else:
#            result = 0i64
#        of binopCmpLt:
#          if ast.myBinop.left.myEval() < ast.myBinop.right.myEval():
#            result = 1i64
#          else:
#            result = 0i64
#        of binopCmpGt:
#          if ast.myBinop.left.myEval() > ast.myBinop.right.myEval():
#            result = 1i64
#          else:
#            result = 0i64
#        of binopCmpLe:
#          if ast.myBinop.left.myEval() <= ast.myBinop.right.myEval():
#            result = 1i64
#          else:
#            result = 0i64
#        of binopCmpGe:
#          if ast.myBinop.left.myEval() >= ast.myBinop.right.myEval():
#            result = 1i64
#          else:
#            result = 0i64
#        #--------
#        of binopPlus:
#          result = (
#            ast.myBinop.left.myEval() + ast.myBinop.right.myEval()
#          )
#        of binopMinus:
#          result = (
#            ast.myBinop.left.myEval() - ast.myBinop.right.myEval()
#          )
#        of binopMul:
#          result = (
#            ast.myBinop.left.myEval() * ast.myBinop.right.myEval()
#          )
#        of binopDiv:
#          result = (
#            ast.myBinop.left.myEval() div ast.myBinop.right.myEval()
#          )
#        of binopMod:
#          result = (
#            ast.myBinop.left.myEval() mod ast.myBinop.right.myEval()
#          )
#        of binopBitAnd:
#          result = (
#            ast.myBinop.left.myEval() and ast.myBinop.right.myEval()
#          )
#        of binopBitOr:
#          result = (
#            ast.myBinop.left.myEval() or ast.myBinop.right.myEval()
#          )
#        of binopBitXor:
#          result = (
#            ast.myBinop.left.myEval() xor ast.myBinop.right.myEval()
#          )
#        of binopLogicAnd:
#          if (
#            (
#              ast.myBinop.left.myEval() != 0i64
#            ) and (
#              ast.myBinop.right.myEval() != 0i64
#            )
#          ):
#            result = 1i64
#          else:
#            result = 0i64
#        of binopLogicOr:
#          if (
#            (
#              ast.myBinop.left.myEval() != 0i64
#            ) or (
#              ast.myBinop.right.myEval() != 0i64
#            )
#          ):
#            result = 1i64
#          else:
#            result = 0i64
#        of binopBitShl:
#          result = (
#            ast.myBinop.left.myEval() shl ast.myBinop.right.myEval()
#          )
#        of binopBitShr:
#          result = (
#            ast.myBinop.left.myEval() shr ast.myBinop.right.myEval()
#          )
#        else:
#          doAssert(
#            false,
#            "eek!"
#          )
#      of astUnop:
#        case ast.myUnop.kind:
#        of unopPlus:
#          result = ast.myUnop.obj.myEval()
#        of unopMinus:
#          result = (-ast.myUnop.obj.myEval())
#        of unopLogicNot:
#          if (ast.myUnop.obj.myEval() != 0i64):
#            result = 1i64
#          else:
#            result = 0i64
#        of unopBitInvert:
#          result = not ast.myUnop.obj.myEval()
#        else:
#          doError()
#      else:
#        doError()
#  result = ast.myEval()
