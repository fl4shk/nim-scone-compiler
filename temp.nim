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
    module*: AstModule
    funcDeclSeq*: seq[AstDef]
    structDeclSeq*: seq[AstStruct]
  AstIdent* = ref AstIdentObj
  AstIdentObj* = object
    strVal*: string
  AstU64Lit* = ref AstU64LitObj
  AstU64LitObj* = object
    u64Val*: uint64
  AstStrLit* = ref AstStrLitObj
  AstStrLitObj* = object
    strLitVal*: string
  AstOpenarrLit* = ref AstOpenarrLitObj
  AstOpenarrLitObj* = object
    openarrLitSeq*: seq[AstExpr]
  AstTrue* = ref AstTrueObj
  AstTrueObj* = object
  AstFalse* = ref AstFalseObj
  AstFalseObj* = object
  AstDeref* = ref AstDerefObj
  AstDerefObj* = object
    obj*: AstExpr
  AstDot* = ref AstDotObj
  AstDotObj* = object
    left*: AstExpr
    right*: AstIdent
  AstVar* = ref AstVarObj
  AstVarObj* = object
    child*: AstVarEtcDeclMost
    optExpr*: Option[AstExpr]
  AstConst* = ref AstConstObj
  AstConstObj* = object
    child*: AstVarEtcDeclMost
    expr*: AstExpr
  AstDef* = ref AstDefObj
  AstDefObj* = object
    ident*: AstIdent
    genericDeclSeq*: seq[AstIdent]
    argDeclSeq*: seq[AstVarEtcDeclMost]
    returnType*: AstType
    stmtSeq*: seq[AstStmt]
  AstModule* = ref AstModuleObj
  AstModuleObj* = object
    ident*: AstIdent
  AstStruct* = ref AstStructObj
  AstStructObj* = object
    ident*: AstIdent
    genericDeclSeq*: seq[AstIdent]
    fieldSeq*: seq[AstVarEtcDeclMost]
  AstEnum* = ref AstEnumObj
  AstEnumObj* = object
  AstVariant* = ref AstVariantObj
  AstVariantObj* = object
  AstExtern* = ref AstExternObj
  AstExternObj* = object
  AstCextern* = ref AstCexternObj
  AstCexternObj* = object
  AstImport* = ref AstImportObj
  AstImportObj* = object
  AstCimport* = ref AstCimportObj
  AstCimportObj* = object
  SubAstScope* = ref SubAstScopeObj
  SubAstScopeObj* = object
    stmtSeq*: seq[AstStmt]
  SubAstIf* = ref SubAstIfObj
  SubAstIfObj* = object
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
    elifSeq*: seq[AstElif]
    optElse*: Option[AstElse]
  AstElif* = ref AstElifObj
  AstElifObj* = object
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  AstElse* = ref AstElseObj
  AstElseObj* = object
    stmtSeq*: seq[AstStmt]
  SubAstSwitch* = ref SubAstSwitchObj
  SubAstSwitchObj* = object
    expr*: AstExpr
    caseSeq*: seq[AstCase]
    optDefault*: Option[AstDefault]
  AstCase* = ref AstCaseObj
  AstCaseObj* = object
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  AstDefault* = ref AstDefaultObj
  AstDefaultObj* = object
    stmtSeq*: seq[AstStmt]
  SubAstFor* = ref SubAstForObj
  SubAstForObj* = object
    ident*: AstIdent
    exprPre*: AstExpr
    exprPost*: AstExpr
    isUntil*: bool
    stmtSeq*: seq[AstStmt]
  SubAstWhile* = ref SubAstWhileObj
  SubAstWhileObj* = object
    expr*: AstExpr
    stmtSeq*: seq[AstStmt]
  SubAstContinue* = ref SubAstContinueObj
  SubAstContinueObj* = object
  SubAstBreak* = ref SubAstBreakObj
  SubAstBreakObj* = object
  SubAstReturn* = ref SubAstReturnObj
  SubAstReturnObj* = object
    optExpr*: Option[AstExpr]
  AstArray* = ref AstArrayObj
  AstArrayObj* = object
    dim*: AstExpr
    elemType*: AstType
  AstOpenarray* = ref AstOpenarrayObj
  AstOpenarrayObj* = object
    elemType*: AstType
  SubAstBuiltinTypeCast* = ref SubAstBuiltinTypeCastObj
  SubAstBuiltinTypeCastObj* = object
    type*: AstType
    obj*: AstExpr
  SubAstExprIdent* = ref SubAstExprIdentObj
  SubAstExprIdentObj* = object
    ident*: AstIdent
  SubAstUnop* = ref SubAstUnopObj
  SubAstUnopObj* = object
    kind*: AstUnopKind
    obj*: AstExpr
  SubAstBinop* = ref SubAstBinopObj
  SubAstBinopObj* = object
    kind*: AstBinopKind
    left*: AstExpr
    right*: AstExpr
  SubAstAssignEtc* = ref SubAstAssignEtcObj
  SubAstAssignEtcObj* = object
    kind*: AstAssignEtcKind
    left*: AstExpr
    right*: AstExpr
  SubAstBasicType* = ref SubAstBasicTypeObj
  SubAstBasicTypeObj* = object
    kind*: AstBasicTypeKind
  SubAstNamedType* = ref SubAstNamedTypeObj
  SubAstNamedTypeObj* = object
    ident*: AstIdent
    genericImplSeq*: seq[AstGenericArgImpl]
  AstType* = ref AstTypeObj
  AstTypeObj* = object
    kwVar*: bool
    ptrDim*: uint64
    child*: AstTypeSub
  SubAstFuncCall* = ref SubAstFuncCallObj
  SubAstFuncCallObj* = object
    ident*: AstIdent
    genericImplSeq*: seq[AstGenericArgImpl]
    argImplSeq*: seq[AstFuncArgImpl]
  AstFuncArgImpl* = ref AstFuncArgImplObj
  AstFuncArgImplObj* = object
    ident*: Option[AstIdent]
    expr*: AstExpr
  AstGenericArgImpl* = ref AstGenericArgImplObj
  AstGenericArgImplObj* = object
    ident*: Option[AstIdent]
    type*: AstType
  SubAstStmtExprLhs* = ref SubAstStmtExprLhsObj
  SubAstStmtExprLhsObj* = object
    expr*: AstExpr
  AstVarEtcDeclMost* = ref AstVarEtcDeclMostObj
  AstVarEtcDeclMostObj* = object
    ident*: AstIdent
    type*: AstType
  AstStmt* = ref AstStmtObj
  AstStmtObj* = object
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
    case kind*: AstTypeSubKind
    of typeSubBasicType:
      myBasicType*: SubAstBasicType
    of typeSubNamedType:
      myNamedType*: SubAstNamedType
