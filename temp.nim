type
  AstStmtKind* = enum
    stmtScope, stmtIf, stmtSwitch, stmtFor, stmtWhile, stmtContinue, stmtBreak,
    stmtReturn, stmtAssignEtc, stmtStmtExprLhs
type
  AstExprKind* = enum
    exprBuiltinTypeCast, exprExprIdent, exprUnop, exprBinop, exprFuncCall
type
  AstTypeSubKind* = enum
    typeSubBasicType, typeSubNamedType
type
  AstSrcFile* = ref AstSrcFileObj
  AstSrcFileObj* = object
    lexMain*: LexMain
    module*: AstModule
    funcDeclSeq*: seq[AstDef]
    structDeclSeq*: seq[AstStruct]
  AstIdent* = ref AstIdentObj
  AstIdentObj* = object
    lexMain*: LexMain
    strVal*: string
  AstU64Lit* = ref AstU64LitObj
  AstU64LitObj* = object
    lexMain*: LexMain
    u64Val*: uint64
  AstStrLit* = ref AstStrLitObj
  AstStrLitObj* = object
    lexMain*: LexMain
    strLitVal*: string
  AstOpenarrLit* = ref AstOpenarrLitObj
  AstOpenarrLitObj* = object
    lexMain*: LexMain
    openarrLitSeq*: seq[AstExpr]
  AstTrue* = ref AstTrueObj
  AstTrueObj* = object
    lexMain*: LexMain
  AstFalse* = ref AstFalseObj
  AstFalseObj* = object
    lexMain*: LexMain
  AstDeref* = ref AstDerefObj
  AstDerefObj* = object
    lexMain*: LexMain
    obj*: AstExpr
  AstDot* = ref AstDotObj
  AstDotObj* = object
    lexMain*: LexMain
    left*: AstExpr
    right*: AstIdent
  AstVar* = ref AstVarObj
  AstVarObj* = object
    lexMain*: LexMain
    child*: AstVarEtcDeclMost
    optExpr*: Option[AstExpr]
  AstConst* = ref AstConstObj
  AstConstObj* = object
    lexMain*: LexMain
    child*: AstVarEtcDeclMost
    expr*: AstExpr
  AstDef* = ref AstDefObj
  AstDefObj* = object
    lexMain*: LexMain
    ident*: AstIdent
    genericDeclSeq*: seq[AstIdent]
    argDeclSeq*: seq[AstVarEtcDeclMost]
    returnType*: AstType
    stmtSeq*: seq[AstStmt]
  AstModule* = ref AstModuleObj
  AstModuleObj* = object
    lexMain*: LexMain
    ident*: AstIdent
  AstStruct* = ref AstStructObj
  AstStructObj* = object
    lexMain*: LexMain
    ident*: AstIdent
    genericDeclSeq*: seq[AstIdent]
    fieldSeq*: seq[AstVarEtcDeclMost]
  AstEnum* = ref AstEnumObj
  AstEnumObj* = object
    lexMain*: LexMain
  AstVariant* = ref AstVariantObj
  AstVariantObj* = object
    lexMain*: LexMain
  AstExtern* = ref AstExternObj
  AstExternObj* = object
    lexMain*: LexMain
  AstCextern* = ref AstCexternObj
  AstCexternObj* = object
    lexMain*: LexMain
  AstImport* = ref AstImportObj
  AstImportObj* = object
    lexMain*: LexMain
  AstCimport* = ref AstCimportObj
  AstCimportObj* = object
    lexMain*: LexMain
  SubAstScope* = ref SubAstScopeObj
  SubAstScopeObj* = object
    lexMain*: LexMain
    stmtSeq*: seq[AstStmt]
  SubAstIf* = ref SubAstIfObj
  SubAstIfObj* = object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
    elifSeq*: seq[AstElif]
    optElse*: Option[AstElse]
  AstElif* = ref AstElifObj
  AstElifObj* = object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  AstElse* = ref AstElseObj
  AstElseObj* = object
    lexMain*: LexMain
    stmtSeq*: seq[AstStmt]
  SubAstSwitch* = ref SubAstSwitchObj
  SubAstSwitchObj* = object
    lexMain*: LexMain
    expr*: AstExpr
    caseSeq*: seq[AstCase]
    optDefault*: Option[AstDefault]
  AstCase* = ref AstCaseObj
  AstCaseObj* = object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  AstDefault* = ref AstDefaultObj
  AstDefaultObj* = object
    lexMain*: LexMain
    stmtSeq*: seq[AstStmt]
  SubAstFor* = ref SubAstForObj
  SubAstForObj* = object
    lexMain*: LexMain
    ident*: AstIdent
    exprPre*: AstExpr
    exprPost*: AstExpr
    isUntil*: bool
    stmtSeq*: seq[AstStmt]
  SubAstWhile* = ref SubAstWhileObj
  SubAstWhileObj* = object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  SubAstContinue* = ref SubAstContinueObj
  SubAstContinueObj* = object
    lexMain*: LexMain
  SubAstBreak* = ref SubAstBreakObj
  SubAstBreakObj* = object
    lexMain*: LexMain
  SubAstReturn* = ref SubAstReturnObj
  SubAstReturnObj* = object
    lexMain*: LexMain
    optExpr*: Option[AstExpr]
  AstArray* = ref AstArrayObj
  AstArrayObj* = object
    lexMain*: LexMain
    dim*: AstExpr
    elemType*: AstType
  AstOpenarray* = ref AstOpenarrayObj
  AstOpenarrayObj* = object
    lexMain*: LexMain
    elemType*: AstType
  SubAstBuiltinTypeCast* = ref SubAstBuiltinTypeCastObj
  SubAstBuiltinTypeCastObj* = object
    lexMain*: LexMain
    type*: AstType
    obj*: AstExpr
  SubAstExprIdent* = ref SubAstExprIdentObj
  SubAstExprIdentObj* = object
    lexMain*: LexMain
    ident*: AstIdent
  SubAstUnop* = ref SubAstUnopObj
  SubAstUnopObj* = object
    lexMain*: LexMain
    kind*: AstUnopKind
    obj*: AstExpr
  SubAstBinop* = ref SubAstBinopObj
  SubAstBinopObj* = object
    lexMain*: LexMain
    kind*: AstBinopKind
    left*: AstExpr
    right*: AstExpr
  SubAstAssignEtc* = ref SubAstAssignEtcObj
  SubAstAssignEtcObj* = object
    lexMain*: LexMain
    kind*: AstAssignEtcKind
    left*: AstExpr
    right*: AstExpr
  SubAstBasicType* = ref SubAstBasicTypeObj
  SubAstBasicTypeObj* = object
    lexMain*: LexMain
    kind*: AstBasicTypeKind
  SubAstNamedType* = ref SubAstNamedTypeObj
  SubAstNamedTypeObj* = object
    lexMain*: LexMain
    ident*: AstIdent
    genericImplSeq*: seq[AstGenericArgImpl]
  AstType* = ref AstTypeObj
  AstTypeObj* = object
    lexMain*: LexMain
    kwVar*: bool
    ptrDim*: uint64
    child*: AstTypeSub
  SubAstFuncCall* = ref SubAstFuncCallObj
  SubAstFuncCallObj* = object
    lexMain*: LexMain
    ident*: AstIdent
    genericImplSeq*: seq[AstGenericArgImpl]
    argImplSeq*: seq[AstFuncArgImpl]
  AstFuncArgImpl* = ref AstFuncArgImplObj
  AstFuncArgImplObj* = object
    lexMain*: LexMain
    ident*: Option[AstIdent]
    expr*: AstExpr
  AstGenericArgImpl* = ref AstGenericArgImplObj
  AstGenericArgImplObj* = object
    lexMain*: LexMain
    ident*: Option[AstIdent]
    type*: AstType
  SubAstStmtExprLhs* = ref SubAstStmtExprLhsObj
  SubAstStmtExprLhsObj* = object
    lexMain*: LexMain
    expr*: AstExpr
  AstVarEtcDeclMost* = ref AstVarEtcDeclMostObj
  AstVarEtcDeclMostObj* = object
    lexMain*: LexMain
    ident*: AstIdent
    type*: AstType
  AstStmt* = ref AstStmtObj
  AstStmtObj* = object
    lexMain*: LexMain
    case kind*: AstStmtKind
    of stmtScope:
      myScope*: SubAstScope
    of stmtIf:
      myIf*: SubAstIf
    of stmtSwitch:
      mySwitch*: SubAstSwitch
    of stmtFor:
      myFor*: SubAstFor
    of stmtWhile:
      myWhile*: SubAstWhile
    of stmtContinue:
      myContinue*: SubAstContinue
    of stmtBreak:
      myBreak*: SubAstBreak
    of stmtReturn:
      myReturn*: SubAstReturn
    of stmtAssignEtc:
      myAssignEtc*: SubAstAssignEtc
    of stmtStmtExprLhs:
      myStmtExprLhs*: SubAstStmtExprLhs
  AstExpr* = ref AstExprObj
  AstExprObj* = object
    lexMain*: LexMain
    case kind*: AstExprKind
    of exprBuiltinTypeCast:
      myBuiltinTypeCast*: SubAstBuiltinTypeCast
    of exprExprIdent:
      myExprIdent*: SubAstExprIdent
    of exprUnop:
      myUnop*: SubAstUnop
    of exprBinop:
      myBinop*: SubAstBinop
    of exprFuncCall:
      myFuncCall*: SubAstFuncCall
  AstTypeSub* = ref AstTypeSubObj
  AstTypeSubObj* = object
    lexMain*: LexMain
    case kind*: AstTypeSubKind
    of typeSubBasicType:
      myBasicType*: SubAstBasicType
    of typeSubNamedType:
      myNamedType*: SubAstNamedType
  AstNode* = ref AstNodeObj
  AstNodeObj* = object
    lexMain*: LexMain
    case kind*: AstKind
    of astSrcFile:
      mySrcFile*: AstSrcFile
    of astIdent:
      myIdent*: AstIdent
    of astU64Lit:
      myU64Lit*: AstU64Lit
    of astStrLit:
      myStrLit*: AstStrLit
    of astOpenarrLit:
      myOpenarrLit*: AstOpenarrLit
    of astTrue:
      myTrue*: AstTrue
    of astFalse:
      myFalse*: AstFalse
    of astDeref:
      myDeref*: AstDeref
    of astDot:
      myDot*: AstDot
    of astVar:
      myVar*: AstVar
    of astConst:
      myConst*: AstConst
    of astDef:
      myDef*: AstDef
    of astModule:
      myModule*: AstModule
    of astStruct:
      myStruct*: AstStruct
    of astEnum:
      myEnum*: AstEnum
    of astVariant:
      myVariant*: AstVariant
    of astExtern:
      myExtern*: AstExtern
    of astCextern:
      myCextern*: AstCextern
    of astImport:
      myImport*: AstImport
    of astCimport:
      myCimport*: AstCimport
    of astElif:
      myElif*: AstElif
    of astElse:
      myElse*: AstElse
    of astCase:
      myCase*: AstCase
    of astDefault:
      myDefault*: AstDefault
    of astArray:
      myArray*: AstArray
    of astOpenarray:
      myOpenarray*: AstOpenarray
    of astType:
      myType*: AstType
    of astFuncArgImpl:
      myFuncArgImpl*: AstFuncArgImpl
    of astGenericArgImpl:
      myGenericArgImpl*: AstGenericArgImpl
    of astVarEtcDeclMost:
      myVarEtcDeclMost*: AstVarEtcDeclMost
    of astStmt:
      myStmt*: AstStmt
    of astExpr:
      myExpr*: AstExpr
    of astTypeSub:
      myTypeSub*: AstTypeSub

proc toAstNode*(obj: AstSrcFile): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astSrcFile, mySrcFile: obj)

proc toAstNode*(obj: AstIdent): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astIdent, myIdent: obj)

proc toAstNode*(obj: AstU64Lit): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astU64Lit, myU64Lit: obj)

proc toAstNode*(obj: AstStrLit): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astStrLit, myStrLit: obj)

proc toAstNode*(obj: AstOpenarrLit): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astOpenarrLit, myOpenarrLit: obj)

proc toAstNode*(obj: AstTrue): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astTrue, myTrue: obj)

proc toAstNode*(obj: AstFalse): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astFalse, myFalse: obj)

proc toAstNode*(obj: AstDeref): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astDeref, myDeref: obj)

proc toAstNode*(obj: AstDot): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astDot, myDot: obj)

proc toAstNode*(obj: AstVar): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astVar, myVar: obj)

proc toAstNode*(obj: AstConst): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astConst, myConst: obj)

proc toAstNode*(obj: AstDef): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astDef, myDef: obj)

proc toAstNode*(obj: AstModule): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astModule, myModule: obj)

proc toAstNode*(obj: AstStruct): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astStruct, myStruct: obj)

proc toAstNode*(obj: AstEnum): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astEnum, myEnum: obj)

proc toAstNode*(obj: AstVariant): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astVariant, myVariant: obj)

proc toAstNode*(obj: AstExtern): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astExtern, myExtern: obj)

proc toAstNode*(obj: AstCextern): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astCextern, myCextern: obj)

proc toAstNode*(obj: AstImport): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astImport, myImport: obj)

proc toAstNode*(obj: AstCimport): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astCimport, myCimport: obj)

proc toAstNode*(obj: AstElif): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astElif, myElif: obj)

proc toAstNode*(obj: AstElse): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astElse, myElse: obj)

proc toAstNode*(obj: AstCase): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astCase, myCase: obj)

proc toAstNode*(obj: AstDefault): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astDefault, myDefault: obj)

proc toAstNode*(obj: AstArray): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astArray, myArray: obj)

proc toAstNode*(obj: AstOpenarray): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astOpenarray, myOpenarray: obj)

proc toAstNode*(obj: AstType): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astType, myType: obj)

proc toAstNode*(obj: AstFuncArgImpl): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astFuncArgImpl,
                   myFuncArgImpl: obj)

proc toAstNode*(obj: AstGenericArgImpl): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astGenericArgImpl,
                   myGenericArgImpl: obj)

proc toAstNode*(obj: AstVarEtcDeclMost): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astVarEtcDeclMost,
                   myVarEtcDeclMost: obj)

proc toAstNode*(obj: AstStmt): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astStmt, myStmt: obj)

proc toAstNode*(obj: AstExpr): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astExpr, myExpr: obj)

proc toAstNode*(obj: AstTypeSub): AstNode =
  result = AstNode(lexMain: obj.lexMain, kind: astTypeSub, myTypeSub: obj)

case ast.kind
of astSrcFile:
  result.add "(AstSrcFile"
  result.add "\n"
  result.add((i & "module" & " ") & (ast.mySrcFile.module.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "funcDeclSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.mySrcFile.funcDeclSeq.len():
    tempSeq.add(ast.mySrcFile.funcDeclSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add((i & "structDeclSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.mySrcFile.structDeclSeq.len():
    tempSeq.add(ast.mySrcFile.structDeclSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add(iFinish & ")")
of astIdent:
  result.add "(AstIdent"
  x = indent
  result.add(" " & "\"" & ast.myIdent.strVal & "\"" & "")
  result.add(")")
of astU64Lit:
  result.add "(AstU64Lit"
  x = indent
  result.add(" " & "u64Val" & " " & $ast.myU64Lit.u64Val & "")
  result.add(")")
of astStrLit:
  result.add "(AstStrLit"
  x = indent
  result.add(" " & "\"" & ast.myStrLit.strLitVal & "\"" & "")
  result.add(")")
of astOpenarrLit:
  result.add "(AstOpenarrLit"
  x = indent
  result.add((" " & "openarrLitSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myOpenarrLit.openarrLitSeq.len():
    tempSeq.add(ast.myOpenarrLit.openarrLitSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("")
  result.add(")")
of astTrue:
  result.add "(AstTrue"
  result.add(")")
of astFalse:
  result.add "(AstFalse"
  result.add(")")
of astDeref:
  result.add "(AstDeref"
  x = indent
  result.add((" " & "obj" & " ") & (ast.myDeref.obj.toAstNode().toStr(x)) & (""))
  result.add(")")
of astDot:
  result.add "(AstDot"
  result.add "\n"
  result.add((i & "left" & " ") & (ast.myDot.left.toAstNode().toStr(x)) & ("\n"))
  result.add((i & "right" & " ") & (ast.myDot.right.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astVar:
  result.add "(AstVar"
  result.add "\n"
  result.add((i & "child" & " ") & (ast.myVar.child.toAstNode().toStr(x)) &
      ("\n"))
  if ast.myVar.optExpr.isSome:
    result.add((i & "optExpr" & " ") &
        (ast.myVar.optExpr.get.toAstNode().toStr(x)) &
        ("\n"))
  else:
    result.add((i & "optExpr" & " " & "!isSome") & ("\n"))
  result.add(iFinish & ")")
of astConst:
  result.add "(AstConst"
  result.add "\n"
  result.add((i & "child" & " ") & (ast.myConst.child.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "expr" & " ") & (ast.myConst.expr.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astDef:
  result.add "(AstDef"
  result.add "\n"
  result.add((i & "ident" & " ") & (ast.myDef.ident.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "genericDeclSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myDef.genericDeclSeq.len():
    tempSeq.add(ast.myDef.genericDeclSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add((i & "argDeclSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myDef.argDeclSeq.len():
    tempSeq.add(ast.myDef.argDeclSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add((i & "returnType" & " ") &
      (ast.myDef.returnType.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "stmtSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myDef.stmtSeq.len():
    tempSeq.add(ast.myDef.stmtSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add(iFinish & ")")
of astModule:
  result.add "(AstModule"
  x = indent
  result.add((" " & "ident" & " ") & (ast.myModule.ident.toAstNode().toStr(x)) &
      (""))
  result.add(")")
of astStruct:
  result.add "(AstStruct"
  result.add "\n"
  result.add((i & "ident" & " ") & (ast.myStruct.ident.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "genericDeclSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myStruct.genericDeclSeq.len():
    tempSeq.add(ast.myStruct.genericDeclSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add((i & "fieldSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myStruct.fieldSeq.len():
    tempSeq.add(ast.myStruct.fieldSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add(iFinish & ")")
of astEnum:
  result.add "(AstEnum"
  result.add(")")
of astVariant:
  result.add "(AstVariant"
  result.add(")")
of astExtern:
  result.add "(AstExtern"
  result.add(")")
of astCextern:
  result.add "(AstCextern"
  result.add(")")
of astImport:
  result.add "(AstImport"
  result.add(")")
of astCimport:
  result.add "(AstCimport"
  result.add(")")
of astElif:
  result.add "(AstElif"
  result.add "\n"
  result.add((i & "expr" & " ") & (ast.myElif.expr.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "stmtSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myElif.stmtSeq.len():
    tempSeq.add(ast.myElif.stmtSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add(iFinish & ")")
of astElse:
  result.add "(AstElse"
  x = indent
  result.add((" " & "stmtSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myElse.stmtSeq.len():
    tempSeq.add(ast.myElse.stmtSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("")
  result.add(")")
of astCase:
  result.add "(AstCase"
  result.add "\n"
  result.add((i & "expr" & " ") & (ast.myCase.expr.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "stmtSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myCase.stmtSeq.len():
    tempSeq.add(ast.myCase.stmtSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("\n")
  result.add(iFinish & ")")
of astDefault:
  result.add "(AstDefault"
  x = indent
  result.add((" " & "stmtSeq" & " "))
  tempSeq.setLen(0)
  for kdx in 0 ..< ast.myDefault.stmtSeq.len():
    tempSeq.add(ast.myDefault.stmtSeq[kdx].toAstNode())
  result.add tempSeq.toStr(x)
  result.add("")
  result.add(")")
of astArray:
  result.add "(AstArray"
  result.add "\n"
  result.add((i & "dim" & " ") & (ast.myArray.dim.toAstNode().toStr(x)) & ("\n"))
  result.add((i & "elemType" & " ") &
      (ast.myArray.elemType.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astOpenarray:
  result.add "(AstOpenarray"
  x = indent
  result.add((" " & "elemType" & " ") &
      (ast.myOpenarray.elemType.toAstNode().toStr(x)) &
      (""))
  result.add(")")
of astType:
  result.add "(AstType"
  result.add "\n"
  result.add(i & "kwVar" & " " & $ast.myType.kwVar & "\n")
  result.add(i & "ptrDim" & " " & $ast.myType.ptrDim & "\n")
  result.add((i & "child" & " ") & (ast.myType.child.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astFuncArgImpl:
  result.add "(AstFuncArgImpl"
  result.add "\n"
  if ast.myFuncArgImpl.ident.isSome:
    result.add((i & "ident" & " ") &
        (ast.myFuncArgImpl.ident.get.toAstNode().toStr(x)) &
        ("\n"))
  else:
    result.add((i & "ident" & " " & "!isSome") & ("\n"))
  result.add((i & "expr" & " ") & (ast.myFuncArgImpl.expr.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astGenericArgImpl:
  result.add "(AstGenericArgImpl"
  result.add "\n"
  if ast.myGenericArgImpl.ident.isSome:
    result.add((i & "ident" & " ") &
        (ast.myGenericArgImpl.ident.get.toAstNode().toStr(x)) &
        ("\n"))
  else:
    result.add((i & "ident" & " " & "!isSome") & ("\n"))
  result.add((i & "type" & " ") &
      (ast.myGenericArgImpl.type.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astVarEtcDeclMost:
  result.add "(AstVarEtcDeclMost"
  result.add "\n"
  result.add((i & "ident" & " ") &
      (ast.myVarEtcDeclMost.ident.toAstNode().toStr(x)) &
      ("\n"))
  result.add((i & "type" & " ") &
      (ast.myVarEtcDeclMost.type.toAstNode().toStr(x)) &
      ("\n"))
  result.add(iFinish & ")")
of astStmt:
  case ast.myStmt.kind
  of stmtScope:
    result.add "(SubAstScope"
    x = indent
    result.add((" " & "stmtSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myStmt.myScope.stmtSeq.len():
      tempSeq.add(ast.myStmt.myScope.stmtSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("")
    result.add(")")
  of stmtIf:
    result.add "(SubAstIf"
    result.add "\n"
    result.add((i & "expr" & " ") & (ast.myStmt.myIf.expr.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "stmtSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myStmt.myIf.stmtSeq.len():
      tempSeq.add(ast.myStmt.myIf.stmtSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    result.add((i & "elifSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myStmt.myIf.elifSeq.len():
      tempSeq.add(ast.myStmt.myIf.elifSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    if ast.myStmt.myIf.optElse.isSome:
      result.add((i & "optElse" & " ") &
          (ast.myStmt.myIf.optElse.get.toAstNode().toStr(x)) &
          ("\n"))
    else:
      result.add((i & "optElse" & " " & "!isSome") & ("\n"))
    result.add(iFinish & ")")
  of stmtSwitch:
    result.add "(SubAstSwitch"
    result.add "\n"
    result.add((i & "expr" & " ") &
        (ast.myStmt.mySwitch.expr.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "caseSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myStmt.mySwitch.caseSeq.len():
      tempSeq.add(ast.myStmt.mySwitch.caseSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    if ast.myStmt.mySwitch.optDefault.isSome:
      result.add((i & "optDefault" & " ") &
          (ast.myStmt.mySwitch.optDefault.get.toAstNode().toStr(x)) &
          ("\n"))
    else:
      result.add((i & "optDefault" & " " & "!isSome") & ("\n"))
    result.add(iFinish & ")")
  of stmtFor:
    result.add "(SubAstFor"
    result.add "\n"
    result.add((i & "ident" & " ") &
        (ast.myStmt.myFor.ident.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "exprPre" & " ") &
        (ast.myStmt.myFor.exprPre.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "exprPost" & " ") &
        (ast.myStmt.myFor.exprPost.toAstNode().toStr(x)) &
        ("\n"))
    result.add(i & "isUntil" & " " & $ast.myStmt.myFor.isUntil & "\n")
    result.add((i & "stmtSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myStmt.myFor.stmtSeq.len():
      tempSeq.add(ast.myStmt.myFor.stmtSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    result.add(iFinish & ")")
  of stmtWhile:
    result.add "(SubAstWhile"
    result.add "\n"
    result.add((i & "expr" & " ") &
        (ast.myStmt.myWhile.expr.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "stmtSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myStmt.myWhile.stmtSeq.len():
      tempSeq.add(ast.myStmt.myWhile.stmtSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    result.add(iFinish & ")")
  of stmtContinue:
    result.add "(SubAstContinue"
    result.add(")")
  of stmtBreak:
    result.add "(SubAstBreak"
    result.add(")")
  of stmtReturn:
    result.add "(SubAstReturn"
    x = indent
    if ast.myStmt.myReturn.optExpr.isSome:
      result.add((" " & "optExpr" & " ") &
          (ast.myStmt.myReturn.optExpr.get.toAstNode().toStr(x)) &
          (""))
    else:
      result.add((" " & "optExpr" & " " & "!isSome") & (""))
    result.add(")")
  of stmtAssignEtc:
    result.add "(SubAstAssignEtc"
    result.add "\n"
    result.add(i & "kind" & " " & $ast.myStmt.myAssignEtc.kind & "\n")
    result.add((i & "left" & " ") &
        (ast.myStmt.myAssignEtc.left.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "right" & " ") &
        (ast.myStmt.myAssignEtc.right.toAstNode().toStr(x)) &
        ("\n"))
    result.add(iFinish & ")")
  of stmtStmtExprLhs:
    result.add "(SubAstStmtExprLhs"
    x = indent
    result.add((" " & "expr" & " ") &
        (ast.myStmt.myStmtExprLhs.expr.toAstNode().toStr(x)) &
        (""))
    result.add(")")
of astExpr:
  case ast.myExpr.kind
  of exprBuiltinTypeCast:
    result.add "(SubAstBuiltinTypeCast"
    result.add "\n"
    result.add((i & "type" & " ") &
        (ast.myExpr.myBuiltinTypeCast.type.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "obj" & " ") &
        (ast.myExpr.myBuiltinTypeCast.obj.toAstNode().toStr(x)) &
        ("\n"))
    result.add(iFinish & ")")
  of exprExprIdent:
    result.add "(SubAstExprIdent"
    x = indent
    result.add((" " & "ident" & " ") &
        (ast.myExpr.myExprIdent.ident.toAstNode().toStr(x)) &
        (""))
    result.add(")")
  of exprUnop:
    result.add "(SubAstUnop"
    result.add "\n"
    result.add(i & "kind" & " " & $ast.myExpr.myUnop.kind & "\n")
    result.add((i & "obj" & " ") & (ast.myExpr.myUnop.obj.toAstNode().toStr(x)) &
        ("\n"))
    result.add(iFinish & ")")
  of exprBinop:
    result.add "(SubAstBinop"
    result.add "\n"
    result.add(i & "kind" & " " & $ast.myExpr.myBinop.kind & "\n")
    result.add((i & "left" & " ") &
        (ast.myExpr.myBinop.left.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "right" & " ") &
        (ast.myExpr.myBinop.right.toAstNode().toStr(x)) &
        ("\n"))
    result.add(iFinish & ")")
  of exprFuncCall:
    result.add "(SubAstFuncCall"
    result.add "\n"
    result.add((i & "ident" & " ") &
        (ast.myExpr.myFuncCall.ident.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "genericImplSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myExpr.myFuncCall.genericImplSeq.len():
      tempSeq.add(ast.myExpr.myFuncCall.genericImplSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    result.add((i & "argImplSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myExpr.myFuncCall.argImplSeq.len():
      tempSeq.add(ast.myExpr.myFuncCall.argImplSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    result.add(iFinish & ")")
of astTypeSub:
  case ast.myTypeSub.kind
  of typeSubBasicType:
    result.add "(SubAstBasicType"
    x = indent
    result.add(" " & "kind" & " " & $ast.myTypeSub.myBasicType.kind & "")
    result.add(")")
  of typeSubNamedType:
    result.add "(SubAstNamedType"
    result.add "\n"
    result.add((i & "ident" & " ") &
        (ast.myTypeSub.myNamedType.ident.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "genericImplSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myTypeSub.myNamedType.genericImplSeq.len():
      tempSeq.add(ast.myTypeSub.myNamedType.genericImplSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("\n")
    result.add(iFinish & ")")
