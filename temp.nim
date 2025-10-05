type
  AstStmtKind* = enum
    stmtScope, stmtIf, stmtSwitch, stmtFor, stmtWhile, stmtContinue, stmtBreak,
    stmtReturn, stmtAssignEtc, stmtStmtExprLhs
type
  AstExprKind* = enum
    exprU64Lit, exprStrLit, exprOpenarrLit, exprTrue, exprFalse, exprDeref,
    exprDot, exprBuiltinTypeCast, exprExprIdent, exprUnop, exprBinop,
    exprFuncCall
type
  AstTypeSubKind* = enum
    typeSubBasicType, typeSubNamedType
type
  AstSrcFile* = ref object
    lexMain*: LexMain
    module*: AstModule
    funcDeclSeq*: seq[AstDef]
    structDeclSeq*: seq[AstStruct]
  AstIdent* = ref object
    lexMain*: LexMain
    strVal*: string
  SubAstU64Lit* = object
    lexMain*: LexMain
    u64Val*: uint64
  SubAstStrLit* = object
    lexMain*: LexMain
    strLitVal*: string
  SubAstOpenarrLit* = object
    lexMain*: LexMain
    openarrLitSeq*: seq[AstExpr]
  SubAstTrue* = object
    lexMain*: LexMain
  SubAstFalse* = object
    lexMain*: LexMain
  SubAstDeref* = object
    lexMain*: LexMain
    obj*: AstExpr
  SubAstDot* = object
    lexMain*: LexMain
    left*: AstExpr
    right*: AstIdent
  AstVar* = ref object
    lexMain*: LexMain
    child*: AstVarEtcDeclMost
    optExpr*: Option[AstExpr]
  AstConst* = ref object
    lexMain*: LexMain
    child*: AstVarEtcDeclMost
    expr*: AstExpr
  AstDef* = ref object
    lexMain*: LexMain
    ident*: AstIdent
    genericDeclSeq*: seq[AstIdent]
    argDeclSeq*: seq[AstVarEtcDeclMost]
    returnType*: AstType
    stmtSeq*: seq[AstStmt]
  AstModule* = ref object
    lexMain*: LexMain
    ident*: AstIdent
  AstStruct* = ref object
    lexMain*: LexMain
    ident*: AstIdent
    genericDeclSeq*: seq[AstIdent]
    fieldSeq*: seq[AstVarEtcDeclMost]
  AstEnum* = ref object
    lexMain*: LexMain
  AstVariant* = ref object
    lexMain*: LexMain
  AstExtern* = ref object
    lexMain*: LexMain
  AstCextern* = ref object
    lexMain*: LexMain
  AstImport* = ref object
    lexMain*: LexMain
  AstCimport* = ref object
    lexMain*: LexMain
  SubAstScope* = object
    lexMain*: LexMain
    stmtSeq*: seq[AstStmt]
  SubAstIf* = object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
    elifSeq*: seq[AstElif]
    optElse*: Option[AstElse]
  AstElif* = ref object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  AstElse* = ref object
    lexMain*: LexMain
    stmtSeq*: seq[AstStmt]
  SubAstSwitch* = object
    lexMain*: LexMain
    expr*: AstExpr
    caseSeq*: seq[AstCase]
    optDefault*: Option[AstDefault]
  AstCase* = ref object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  AstDefault* = ref object
    lexMain*: LexMain
    stmtSeq*: seq[AstStmt]
  SubAstFor* = object
    lexMain*: LexMain
    ident*: AstIdent
    exprPre*: AstExpr
    exprPost*: AstExpr
    isUntil*: bool
    stmtSeq*: seq[AstStmt]
  SubAstWhile* = object
    lexMain*: LexMain
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  SubAstContinue* = object
    lexMain*: LexMain
  SubAstBreak* = object
    lexMain*: LexMain
  SubAstReturn* = object
    lexMain*: LexMain
    optExpr*: Option[AstExpr]
  AstArray* = ref object
    lexMain*: LexMain
    dim*: AstExpr
    elemType*: AstType
  AstOpenarray* = ref object
    lexMain*: LexMain
    elemType*: AstType
  SubAstBuiltinTypeCast* = object
    lexMain*: LexMain
    type*: AstType
    obj*: AstExpr
  SubAstExprIdent* = object
    lexMain*: LexMain
    ident*: AstIdent
  SubAstUnop* = object
    lexMain*: LexMain
    kind*: AstUnopKind
    obj*: AstExpr
  SubAstBinop* = object
    lexMain*: LexMain
    kind*: AstBinopKind
    left*: AstExpr
    right*: AstExpr
  SubAstAssignEtc* = object
    lexMain*: LexMain
    kind*: AstAssignEtcKind
    left*: AstExpr
    right*: AstExpr
  SubAstBasicType* = object
    lexMain*: LexMain
    kind*: AstBasicTypeKind
  SubAstNamedType* = object
    lexMain*: LexMain
    ident*: AstIdent
    genericImplSeq*: seq[AstGenericArgImpl]
  AstType* = ref object
    lexMain*: LexMain
    kwVar*: bool
    ptrDim*: uint64
    child*: AstTypeSub
  SubAstFuncCall* = object
    lexMain*: LexMain
    ident*: AstIdent
    genericImplSeq*: seq[AstGenericArgImpl]
    argImplSeq*: seq[AstFuncArgImpl]
  AstFuncArgImpl* = ref object
    lexMain*: LexMain
    ident*: Option[AstIdent]
    expr*: AstExpr
  AstGenericArgImpl* = ref object
    lexMain*: LexMain
    ident*: Option[AstIdent]
    type*: AstType
  SubAstStmtExprLhs* = object
    lexMain*: LexMain
    expr*: AstExpr
  AstVarEtcDeclMost* = ref object
    lexMain*: LexMain
    ident*: AstIdent
    type*: AstType
  AstStmt* = ref object
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
  AstExpr* = ref object
    lexMain*: LexMain
    case kind*: AstExprKind
    of exprU64Lit:
      myU64Lit*: SubAstU64Lit
    of exprStrLit:
      myStrLit*: SubAstStrLit
    of exprOpenarrLit:
      myOpenarrLit*: SubAstOpenarrLit
    of exprTrue:
      myTrue*: SubAstTrue
    of exprFalse:
      myFalse*: SubAstFalse
    of exprDeref:
      myDeref*: SubAstDeref
    of exprDot:
      myDot*: SubAstDot
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
  AstTypeSub* = ref object
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

proc toAstNode*(obj: AstSrcFile; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astSrcFile, mySrcFile: obj)
  result.mySrcFile.lexMain = lexMain

proc toAstNode*(obj: AstSrcFile): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstIdent; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astIdent, myIdent: obj)
  result.myIdent.lexMain = lexMain

proc toAstNode*(obj: AstIdent): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstVar; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astVar, myVar: obj)
  result.myVar.lexMain = lexMain

proc toAstNode*(obj: AstVar): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstConst; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astConst, myConst: obj)
  result.myConst.lexMain = lexMain

proc toAstNode*(obj: AstConst): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstDef; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astDef, myDef: obj)
  result.myDef.lexMain = lexMain

proc toAstNode*(obj: AstDef): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstModule; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astModule, myModule: obj)
  result.myModule.lexMain = lexMain

proc toAstNode*(obj: AstModule): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstStruct; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astStruct, myStruct: obj)
  result.myStruct.lexMain = lexMain

proc toAstNode*(obj: AstStruct): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstEnum; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astEnum, myEnum: obj)
  result.myEnum.lexMain = lexMain

proc toAstNode*(obj: AstEnum): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstVariant; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astVariant, myVariant: obj)
  result.myVariant.lexMain = lexMain

proc toAstNode*(obj: AstVariant): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstExtern; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astExtern, myExtern: obj)
  result.myExtern.lexMain = lexMain

proc toAstNode*(obj: AstExtern): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstCextern; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astCextern, myCextern: obj)
  result.myCextern.lexMain = lexMain

proc toAstNode*(obj: AstCextern): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstImport; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astImport, myImport: obj)
  result.myImport.lexMain = lexMain

proc toAstNode*(obj: AstImport): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstCimport; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astCimport, myCimport: obj)
  result.myCimport.lexMain = lexMain

proc toAstNode*(obj: AstCimport): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstElif; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astElif, myElif: obj)
  result.myElif.lexMain = lexMain

proc toAstNode*(obj: AstElif): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstElse; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astElse, myElse: obj)
  result.myElse.lexMain = lexMain

proc toAstNode*(obj: AstElse): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstCase; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astCase, myCase: obj)
  result.myCase.lexMain = lexMain

proc toAstNode*(obj: AstCase): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstDefault; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astDefault, myDefault: obj)
  result.myDefault.lexMain = lexMain

proc toAstNode*(obj: AstDefault): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstArray; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astArray, myArray: obj)
  result.myArray.lexMain = lexMain

proc toAstNode*(obj: AstArray): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstOpenarray; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astOpenarray, myOpenarray: obj)
  result.myOpenarray.lexMain = lexMain

proc toAstNode*(obj: AstOpenarray): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstType; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astType, myType: obj)
  result.myType.lexMain = lexMain

proc toAstNode*(obj: AstType): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstFuncArgImpl; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astFuncArgImpl, myFuncArgImpl: obj)
  result.myFuncArgImpl.lexMain = lexMain

proc toAstNode*(obj: AstFuncArgImpl): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstGenericArgImpl; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astGenericArgImpl,
                   myGenericArgImpl: obj)
  result.myGenericArgImpl.lexMain = lexMain

proc toAstNode*(obj: AstGenericArgImpl): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstVarEtcDeclMost; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astVarEtcDeclMost,
                   myVarEtcDeclMost: obj)
  result.myVarEtcDeclMost.lexMain = lexMain

proc toAstNode*(obj: AstVarEtcDeclMost): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstStmt; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astStmt, myStmt: obj)
  result.myStmt.lexMain = lexMain

proc toAstNode*(obj: AstStmt): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstExpr; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astExpr, myExpr: obj)
  result.myExpr.lexMain = lexMain

proc toAstNode*(obj: AstExpr): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

proc toAstNode*(obj: AstTypeSub; lexMain: LexMain): AstNode =
  result = AstNode(lexMain: lexMain, kind: astTypeSub, myTypeSub: obj)
  result.myTypeSub.lexMain = lexMain

proc toAstNode*(obj: AstTypeSub): AstNode =
  result = toAstNode(obj = obj, lexMain = obj.lexMain)

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
  of exprU64Lit:
    result.add "(SubAstU64Lit"
    x = indent
    result.add(" " & "u64Val" & " " & $ast.myExpr.myU64Lit.u64Val & "")
    result.add(")")
  of exprStrLit:
    result.add "(SubAstStrLit"
    x = indent
    result.add(" " & "\"" & ast.myExpr.myStrLit.strLitVal & "\"" & "")
    result.add(")")
  of exprOpenarrLit:
    result.add "(SubAstOpenarrLit"
    x = indent
    result.add((" " & "openarrLitSeq" & " "))
    tempSeq.setLen(0)
    for kdx in 0 ..< ast.myExpr.myOpenarrLit.openarrLitSeq.len():
      tempSeq.add(ast.myExpr.myOpenarrLit.openarrLitSeq[kdx].toAstNode())
    result.add tempSeq.toStr(x)
    result.add("")
    result.add(")")
  of exprTrue:
    result.add "(SubAstTrue"
    result.add(")")
  of exprFalse:
    result.add "(SubAstFalse"
    result.add(")")
  of exprDeref:
    result.add "(SubAstDeref"
    x = indent
    result.add((" " & "obj" & " ") &
        (ast.myExpr.myDeref.obj.toAstNode().toStr(x)) &
        (""))
    result.add(")")
  of exprDot:
    result.add "(SubAstDot"
    result.add "\n"
    result.add((i & "left" & " ") & (ast.myExpr.myDot.left.toAstNode().toStr(x)) &
        ("\n"))
    result.add((i & "right" & " ") &
        (ast.myExpr.myDot.right.toAstNode().toStr(x)) &
        ("\n"))
    result.add(iFinish & ")")
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
