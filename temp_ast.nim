type
  AstSrcFile* = object
    module*: AstNode
    funcDeclSeq*: seq[AstNode]
    structDeclSeq*: seq[AstNode]
  AstIdent* = object
    strVal*: string
  AstU64Lit* = object
    u64Val*: uint64
  AstStrLit* = object
    strLitVal*: string
  AstTrue* = object
  AstFalse* = object
  AstDeref* = object
    obj*: AstNode
  AstDot* = object
    left*: AstNode
    right*: AstNode
  AstVar* = object
    child*: AstNode
    optExpr*: Option[AstNode]
  AstConst* = object
    child*: AstNode
    expr*: AstNode
  AstDef* = object
    ident*: AstNode
    genericDecl*: Option[AstNode]
    argDeclSeq*: seq[AstNode]
    returnType*: AstNode
    stmtSeq*: seq[AstNode]
  AstModule* = object
    ident*: AstNode
  AstStruct* = object
    ident*: AstNode
    genericDecl*: Option[AstNode]
    fieldSeq*: seq[AstNode]
  AstEnum* = object
  AstExtern* = object
  AstCextern* = object
  AstImport* = object
  AstCImport* = object
  AstScope* = object
    stmtSeq*: seq[AstNode]
  AstIf* = object
    expr*: AstNode
    stmtSeq*: seq[AstNode]
    elifSeq*: seq[AstNode]
    optElse*: Option[AstNode]
  AstElif* = object
    expr*: AstNode
    stmtSeq*: seq[AstNode]
  AstElse* = object
    stmtSeq*: seq[AstNode]
  AstSwitch* = object
    expr*: AstNode
    caseSeq*: seq[AstNode]
    optDefault*: Option[AstNode]
  AstCase* = object
    expr*: AstNode
    stmtSeq*: seq[AstNode]
  AstDefault* = object
    stmtSeq*: seq[AstNode]
  AstFor* = object
    ident*: AstNode
    exprPre*: AstNode
    exprPost*: AstNode
    isUntil*: bool
    stmtSeq*: seq[AstNode]
  AstWhile* = object
    expr*: AstNode
    stmtSeq*: seq[AstNode]
  AstContinue* = object
  AstBreak* = object
  AstReturn* = object
    optExpr*: Option[AstNode]
  AstArray* = object
    dim*: AstNode
    elemType*: AstNode
  AstUnop* = object
    kind*: AstUnopKind
    obj*: AstNode
  AstBinop* = object
    kind*: AstBinopKind
    left*: AstNode
    right*: AstNode
  AstAssignEtc* = object
    kind*: AstAssignEtcKind
    left*: AstNode
    right*: AstNode
  AstBasicType* = object
    kind*: AstBasicTypeKind
  AstNamedType* = object
    ident*: AstNode
    genericImpl*: AstNode
  AstType* = object
    kwVar*: bool
    ptrDim*: uint64
    child*: AstNode
  AstFuncCall* = object
    ident*: AstNode
    genericImpl*: AstNode
    argImplSeq*: seq[AstNode]
  AstStmtExprLhs* = object
    expr*: AstNode
  AstFuncNamedArgImpl* = object
    ident*: AstNode
    expr*: AstNode
  AstGenericNamedArgImpl* = object
    ident*: AstNode
    type*: AstNode
  AstGenericList* = object
    mySeq*: seq[AstNode]
  AstVarEtcDeclMost* = object
    ident*: AstNode
    type*: AstNode
  AstNode* = ref AstNodeObj
  AstNodeObj* = object
    lexMain*: LexMain
    typeInfo*: Option[TypeInfo]
    case kind*: AstKind
    of astSrcFile:
      mySrcFile*: AstSrcFile
    of astIdent:
      myIdent*: AstIdent
    of astU64Lit:
      myU64Lit*: AstU64Lit
    of astStrLit:
      myStrLit*: AstStrLit
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
    of astExtern:
      myExtern*: AstExtern
    of astCextern:
      myCextern*: AstCextern
    of astImport:
      myImport*: AstImport
    of astCImport:
      myCImport*: AstCImport
    of astScope:
      myScope*: AstScope
    of astIf:
      myIf*: AstIf
    of astElif:
      myElif*: AstElif
    of astElse:
      myElse*: AstElse
    of astSwitch:
      mySwitch*: AstSwitch
    of astCase:
      myCase*: AstCase
    of astDefault:
      myDefault*: AstDefault
    of astFor:
      myFor*: AstFor
    of astWhile:
      myWhile*: AstWhile
    of astContinue:
      myContinue*: AstContinue
    of astBreak:
      myBreak*: AstBreak
    of astReturn:
      myReturn*: AstReturn
    of astArray:
      myArray*: AstArray
    of astUnop:
      myUnop*: AstUnop
    of astBinop:
      myBinop*: AstBinop
    of astAssignEtc:
      myAssignEtc*: AstAssignEtc
    of astBasicType:
      myBasicType*: AstBasicType
    of astNamedType:
      myNamedType*: AstNamedType
    of astType:
      myType*: AstType
    of astFuncCall:
      myFuncCall*: AstFuncCall
    of astStmtExprLhs:
      myStmtExprLhs*: AstStmtExprLhs
    of astFuncNamedArgImpl:
      myFuncNamedArgImpl*: AstFuncNamedArgImpl
    of astGenericNamedArgImpl:
      myGenericNamedArgImpl*: AstGenericNamedArgImpl
    of astGenericList:
      myGenericList*: AstGenericList
    of astVarEtcDeclMost:
      myVarEtcDeclMost*: AstVarEtcDeclMost
