import std/options
import std/macros
import std/sets

type
  Mode* = enum
    mdOneFile,

type
  AstValKind* = enum
    astValAstNode,
    astValSeqAstNode,
    astValOptAstNode,
    astValString,
    astValU64,
    astValBool,
    astValUnopKind,
    astValBinopKind,
    astValAssignEtcKind,
    astValBasicTypeKind,

  AstUnopKind* = enum
    unopPlus,
    unopMinus,
    unopLogicNot,
    unopBitInvert,
    unopAddr,

  AstBinopKind* = enum
    binopCmpEq,
    binopCmpNe,
    binopCmpLt,
    binopCmpGt,
    binopCmpLe,
    binopCmpGe,
    #--------
    binopPlus,
    binopMinus,
    binopMul,
    binopDiv,
    binopMod,
    binopBitAnd,
    binopBitOr,
    binopBitXor,
    binopLogicAnd,
    binopLogicOr,
    binopBitShl,
    binopBitShr,

  AstAssignEtcKind* = enum
    assignEtcRegular,
    assignEtcPlus,
    assignEtcMinus,
    assignEtcMul,
    assignEtcDiv,
    assignEtcMod,
    assignEtcBitAnd,
    assignEtcBitOr,
    assignEtcBitXor,
    assignEtcBitShl,
    assignEtcBitShr,

  AstBasicTypeKind* = enum
    basicTypeVoid,
    basicTypeBool,
    basicTypeU8,
    basicTypeI8,
    basicTypeU16,
    basicTypeI16,
    basicTypeU32,
    basicTypeI32,
    basicTypeU64,
    basicTypeI64,
    basicTypeF32,
    basicTypeF64,
    basicTypeChar,
    basicTypeString,

  #AstOtherExprOpKind* = enum
  #  otherExprOpDot,

  AstExprOpKind* = enum
    exprOpUnop,
    exprOpBinop,
    exprOpDerefDotCallEtc,
    #exprOpOther,
    #opAssignEtc,

  AstExprOp* = object
    kind*: AstExprOpKind
    myUnop*: AstUnopKind
    myBinop*: AstBinopKind
    #case kind*: AstExprOpKind
    #of exprOpUnop: myUnop*: AstUnopKind
    #of exprOpBinop: myBinop*: AstBinopKind
    ##of exprOpOther: myOther*: bool
    ##of opAssignEtc: myAssignEtc*: AstAssignEtcKind

const `exprOpPrioSeq`*: seq[HashSet[AstExprOp]] = @[
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopLogicOr),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopLogicAnd),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopBitOr),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopBitXor),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopBitAnd),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopCmpEq),
    AstExprOp(kind: exprOpBinop, myBinop: binopCmpNe),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopCmpLt),
    AstExprOp(kind: exprOpBinop, myBinop: binopCmpGt),
    AstExprOp(kind: exprOpBinop, myBinop: binopCmpLe),
    AstExprOp(kind: exprOpBinop, myBinop: binopCmpGe),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopBitShl),
    AstExprOp(kind: exprOpBinop, myBinop: binopBitShr),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopPlus),
    AstExprOp(kind: exprOpBinop, myBinop: binopMinus),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpBinop, myBinop: binopMul),
    AstExprOp(kind: exprOpBinop, myBinop: binopDiv),
    AstExprOp(kind: exprOpBinop, myBinop: binopMod),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpUnop, myUnop: unopPlus),
    AstExprOp(kind: exprOpUnop, myUnop: unopMinus),
    AstExprOp(kind: exprOpUnop, myUnop: unopLogicNot),
    AstExprOp(kind: exprOpUnop, myUnop: unopBitInvert),
    AstExprOp(kind: exprOpUnop, myUnop: unopAddr),
  ]),
  toHashSet([
    AstExprOp(kind: exprOpDerefDotCallEtc),
  ]),
]

proc `cmpPrio`*(
  a: AstExprOp,
  b: AstExprOp,
): int =
  #var aPrio: int = 0
  #var bPrio: int = 0
  #for idx in 0 ..< binopPrioSeq.len():
  #  if a in binopPrioSeq[idx]:
  #    aPrio = idx
  #  if b in binopPrioSeq[idx]:
  #    bPrio = idx
  #if aPrio < bPrio:
  #  return -1
  #elif aPrio == bPrio:
  #  return 0
  #else: # if aPrio > bPrio:
  #  return 1

  var aPrio: int = 0
  var bPrio: int = 0
  for idx in 0 ..< exprOpPrioSeq.len():
    let myHashSet = exprOpPrioSeq[idx]
    for myExprOp in myHashSet:
      case myExprOp.kind:
      of exprOpUnop:
        if a.kind == exprOpUnop and a.myUnop == myExprOp.myUnop:
          aPrio = idx
        if b.kind == exprOpUnop and b.myUnop == myExprOp.myUnop:
          bPrio = idx
      of exprOpBinop:
        if a.kind == exprOpBinop and a.myBinop == myExprOp.myBinop:
          aPrio = idx
        if b.kind == exprOpBinop and b.myBinop == myExprOp.myBinop:
          bPrio = idx
      of exprOpDerefDotCallEtc:
        if a.kind == exprOpDerefDotCallEtc:
          aPrio = idx
        if b.kind == exprOpDerefDotCallEtc:
          bPrio = idx
    #case myExprOp.kind:
    #of exprOpUnop:
    #  discard
    #of exprOpDot:
    #  discard
  if aPrio < bPrio:
    return -1
  elif aPrio == bPrio:
    return 0
  else: # if aPrio > bPrio:
    return 1
  
proc `cmpPrioLt`*(
  a: AstExprOp,
  b: AstExprOp,
): bool =
  result = (a.cmpPrio b) < 0
proc `cmpPrioEq`*(
  a: AstExprOp,
  b: AstExprOp,
): bool =
  result = (a.cmpPrio b) == 0
proc `cmpPrioGt`*(
  a: AstExprOp,
  b: AstExprOp,
): bool =
  result = (a.cmpPrio b) > 0

#dumpTree:
#  type
#    StatementKind* = enum
#      stmtScope,
#      #stmtIf,
#      #stmtElif,
#      #stmtElse,
#      #stmtSwitch,
#      #stmtCase,
#      #stmtDefault,
#      #stmtFor,
#      #stmtWhile,
#      #stmtContinue,
#      #stmtBreak,
#      #stmtReturn,
#      #stmtAssign,
#
#    #StatementScope* = ref StatementScopeObj
#    StatementScope* = object
#      stmtSeq*: seq[Statement] # the list of statements
#
#    #StatementIf* = object
#    #  #("expr", astValAstNode),          # (condition) expression
#    #  #("stmtSeq", astValSeqAstNode),    # the list of statements
#    #  #                                  # within the scope
#    #  #("elifSeq", astValSeqAstNode),    # `AstElif` `seq`
#    #  #("optElse", astValOptAstNode),    # optional `AstElse`
#    #  expr*: AstNode
#
#    Statement* = ref StatementObj
#    StatementObj* = object
#      case kind*: StatementKind
#      of stmtScope: myScope*: StatementScope
#      #of stmtIf: myIf*: StatementIf
#      #of stmtElif: myElif*: StatementElif
#      #of stmtElse: myElse*: StatementElse
#      #of stmtSwitch: mySwitch*: StatementSwitch
#      #of stmtCase: myCase*: StatementCase
#      #of stmtDefault: myDefault*: StatementDefault
#      #of stmtFor: myFor*: StatementFor
#      #of stmtWhile: myWhile*: StatementWhile
#      #of stmtContinue: myContinue*: StatementContinue
#      #of stmtBreak: myBreak*: StatementBreak
#      #of stmtReturn: myReturn*: StatementReturn
#      #of stmtAssign: myAssign*: StatementAssign


#StmtList
#  TypeSection
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "StatementKind"
#      Empty
#      EnumTy
#        Empty
#        Ident "stmtScope"
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "StatementScope"
#      Empty
#      ObjectTy
#        Empty
#        Empty
#        RecList
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "stmtSeq"
#            BracketExpr
#              Ident "seq"
#              Ident "Statement"
#            Empty
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "Statement"
#      Empty
#      RefTy
#        Ident "StatementObj"
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "StatementObj"
#      Empty
#      ObjectTy
#        Empty
#        Empty
#        RecList
#          RecCase
#            IdentDefs
#              Postfix
#                Ident "*"
#                Ident "kind"
#              Ident "StatementKind"
#              Empty
#            OfBranch
#              Ident "stmtScope"
#              IdentDefs
#                Postfix
#                  Ident "*"
#                  Ident "myScope"
#                Ident "StatementScope"
#                Empty


type
  MetaAstKind* = enum
    metaAstNone,
    metaAstStmt,
    metaAstExpr,
    metaAstTypeSub,
    #metaAstTypeMain,
    #metaAstFuncArgImpl,
    #metaAstGenericImpl,
  HelperTokKind* = (
    string, Option[string], bool, MetaAstKind,
    seq[(string, AstValKind, string)]
  )


const helperTokKindSeq*: seq[HelperTokKind] = @[
  #--------
  (
    # fake token indicating the beginning of the AST
    "SrcFile", none(string), true, metaAstNone,
    @[
      ("module", astValAstNode, "Module"),
      ("funcDeclSeq", astValSeqAstNode, "Def"),
      ("structDeclSeq", astValSeqAstNode, "Struct"),
    ]
  ),
  (
    # invalid input!
    "Bad", none(string), false, metaAstNone, @[]
  ),
  (
    # end of file
    "Eof", none(string), false, metaAstNone, @[]
  ),
  #("LineComent", none(string)),    # start of line comment
  #--------
  (
    # identifiers
    "Ident", none(string), true, metaAstNone,
    @[
      ("strVal", astValString, "")
    ]
  ),
  (
    # 0-9, hex numbers, binary numbers, etc.
    "U64Lit", none(string), true, metaAstNone,
    @[
      ("u64Val", astValU64, "")
    ]
  ),
  (
    # string literals
    "StrLit", none(string), true, metaAstNone,
    @[
      ("strLitVal", astValString, "")
    ]
  ),
  (
    "OpenarrLit", some("$("), true, metaAstNone,
    @[
      ("openarrLitSeq", astValSeqAstNode, "Expr"),
    ],
  ),
  #--------
  ("True", some("true"), true, metaAstNone, @[]),
  ("False", some("false"), true, metaAstNone, @[]),
  #--------
  ("LParen", some("("), false, metaAstNone, @[]),
  ("RParen", some(")"), false, metaAstNone, @[]),
  ("LBracket", some("["), false, metaAstNone, @[]),
  ("RBracket", some("]"), false, metaAstNone, @[]),
  ("LBrace", some("{"), false, metaAstNone, @[]),
  ("RBrace", some("}"), false, metaAstNone, @[]),
  ("Comma", some(","), false, metaAstNone, @[]),
  ("Semicolon", some(";"), false, metaAstNone, @[]),
  ("Colon", some(":"), false, metaAstNone, @[]),
  #--------
  ("Ptr", some("ptr"), false, metaAstNone, @[]),
  (
    "Addr", some("addr"), false, metaAstNone, # address of an expression
    @[
      #("obj", astValAstNode)
    ]
  ),
  (
    "Deref", some("@"), true, metaAstNone, # pointer dereference
    @[
      ("obj", astValAstNode, "Expr")
    ]
  ),
  (
    "Dot", some("."), true, metaAstNone,
    @[
      ("left", astValAstNode, "Expr"),
      ("right", astValAstNode, "Ident"),
    ]
  ),
  #--------
  (
    "Var", some("var"), true, metaAstNone,
    @[
      ("child", astValAstNode, "VarEtcDeclMost"),  # `AstVarEtcDeclMost`
      ("optExpr", astValOptAstNode, "Expr"),  # optional expression
    ]
  ),
  (
    "Const", some("const"), true, metaAstNone,
    @[
      ("child", astValAstNode, "VarEtcDeclMost"),         # `AstVarEtcDeclMost`
      ("expr", astValAstNode, "Expr"), # expression
    ]
  ),
  (
    "Def", some("def"), true, metaAstNone,
    @[
      ("ident", astValAstNode, "Ident"),
      ("genericDeclSeq", astValSeqAstNode, "Ident"),
      ("argDeclSeq", astValSeqAstNode, "VarEtcDeclMost"),
      ("returnType", astValAstNode, "Type"),
      ("stmtSeq", astValSeqAstNode, "Stmt"),
    ],
  ),
  ("FuncReturnTypePrefix", some("->"), false, metaAstNone, @[]),
  #("FuncNamedArgListStart", some("$(")),
  #("GenericNamedArgListStart", some("$[")),
  #("GenericArgListStart", some("#[")),
  #("Array", some("array")),
  #("Macro", some("macro")),
      # Maybe save `macro` for the bootstrapped compiler?
      # I'm not sure I outright need macros for this version of the
      # compiler
  (
    "Module", some("module"), true, metaAstNone,
    @[
      ("ident", astValAstNode, "Ident"),             # `AstIdent`
    ],
  ),
  (
    "Struct", some("struct"), true, metaAstNone,
    @[
      ("ident", astValAstNode, "Ident"),
      ("genericDeclSeq", astValSeqAstNode, "Ident"),
      ("fieldSeq", astValSeqAstNode, "VarEtcDeclMost"),
    ],
  ),
  (
    "Enum", some("enum"), true, metaAstNone,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Variant", some("variant"), true, metaAstNone,
    @[
      # TODO: come back to this later
      #("ident", astValAstNode, "Ident"),
      #("genericDeclSeq", astValSeqAstNode, "Ident"),
      #("fieldSeq", astValSeqAstNode)
    ],
  ),
  (
    "Extern", some("extern"), true, metaAstNone,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Cextern", some("cextern"), true, metaAstNone,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Import", some("import"), true, metaAstNone,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Cimport", some("cimport"), true, metaAstNone,
    @[
      # TODO: come back to this later
    ]
  ),
  #--------
  (
    "Scope", some("scope"), true, metaAstStmt,
    @[
      ("stmtSeq", astValSeqAstNode, "Stmt"),    # the list of statements
    ]
  ),
  (
    "If", some("if"), true, metaAstStmt,
    @[
      ("expr", astValAstNode, "Expr"),  # (condition) expression
      ("stmtSeq", astValSeqAstNode, "Stmt"), # the list of statements
                                        # within the scope
      ("elifSeq", astValSeqAstNode, "Elif"),    # `AstElif` `seq`
      ("optElse", astValOptAstNode, "Else"),    # optional `AstElse`
    ],
  ),
  (
    "Elif", some("elif"), true, metaAstNone,
    @[
      ("expr", astValAstNode, "Expr"), # (condition) expression
      ("stmtSeq", astValSeqAstNode, "Stmt"),# the list of statements 
                                      # within the scope
      #("optChild", astValOptAstNode), # optional `AstElif` or `AstElse`
    ],
  ),
  (
    "Else", some("else"), true, metaAstNone,
    @[
      ("stmtSeq", astValSeqAstNode, "Stmt"),  # the list of statements
                                      # within the scope
    ]
  ),
  (
    "Switch", some("switch"), true, metaAstStmt,
    @[
      ("expr", astValAstNode, "Expr"),      # the condition
      ("caseSeq", astValSeqAstNode, "Case"),  # the list of `AstCase` 
      ("optDefault", astValOptAstNode, "Default"),
        # optional `AstDefault`
    ]
  ),
  (
    "Case", some("case"), true, metaAstNone,
    @[
      ("expr", astValAstNode, "Expr"),        # the condition
      ("stmtSeq", astValSeqAstNode, "Stmt"),  # the list of statements
                                      # within the scope
    ],
  ),
  (
    "Default", some("default"), true, metaAstNone,
    @[
      ("stmtSeq", astValSeqAstNode, "Stmt"),# the list of statements
                                      # within the scope
    ]
  ),
  (
    "For", some("for"), true, metaAstStmt,
    @[
      ("ident", astValAstNode, "Ident"),# name of the indexing variable
      ("exprPre", astValAstNode, "Expr"),
      ("exprPost", astValAstNode, "Expr"),
      ("isUntil", astValBool, ""),
      ("stmtSeq", astValSeqAstNode, "Stmt"),
    ],
  ),
  ("In", some("in"), false, metaAstNone, @[]),
  ("To", some("to"), false, metaAstNone, @[]),
  ("Until", some("until"), false, metaAstNone, @[]),
  (
    "While", some("while"), true, metaAstStmt,
    @[
      ("expr", astValAstNode, "Expr"), # (condition) expression
      ("stmtSeq", astValSeqAstNode, "Stmt"),  # the list of statements
                                      # within the scope
    ],
  ),
  ("Continue", some("continue"), true, metaAstStmt, @[]),
  ("Break", some("break"), true, metaAstStmt, @[]),
  #("Result", some("result"), true),
  (
    "Return", some("return"), true, metaAstStmt,
    @[
      ("optExpr", astValOptAstNode, "Expr")
    ]
  ),
  #--------
  #("Type", some("type")),
  (
    "Array", some("array"), true, metaAstNone,
    @[
      ("dim", astValAstNode, "Expr"),
      ("elemType", astValAstNode, "Type"),
    ]
  ),
  (
    "Openarray", some("openarray"), true, metaAstNone,
    @[
      ("elemType", astValAstNode, "Type")
    ],
  ),
  (
    "BuiltinTypeCast", none(string), true, metaAstExpr,
    @[
      ("type", astValAstNode, "Type"),
      ("obj", astValAstNode, "Expr"),
    ],
  ),
  #(
  #  "LenCall", some("len"), true,
  #  @[
  #    ("arg", astValAstNode)
  #  ],
  #),
  ("Void", some("void"), false, metaAstNone, @[]),
  ("Bool", some("bool"), false, metaAstNone, @[]),
  ("U8", some("u8"), false, metaAstNone, @[]),
  ("I8", some("i8"), false, metaAstNone, @[]),
  ("U16", some("u16"), false, metaAstNone, @[]),
  ("I16", some("i16"), false, metaAstNone, @[]),
  ("U32", some("u32"), false, metaAstNone, @[]),
  ("I32", some("i32"), false, metaAstNone, @[]),
  ("U64", some("u64"), false, metaAstNone, @[]),
  ("I64", some("i64"), false, metaAstNone, @[]),
  ("F32", some("f32"), false, metaAstNone, @[]),
  ("F64", some("f64"), false, metaAstNone, @[]),
  ("Char", some("char"), false, metaAstNone, @[]),
  ("String", some("string"), false, metaAstNone, @[]),
  #--------
  ("CmpEq", some("=="), false, metaAstNone, @[]),
  ("CmpNe", some("!="), false, metaAstNone, @[]),
  ("CmpLt", some("<"), false, metaAstNone, @[]),
  ("CmpGt", some(">"), false, metaAstNone, @[]),
  ("CmpLe", some("<="), false, metaAstNone, @[]),
  ("CmpGe", some(">="), false, metaAstNone, @[]),
  #--------
  ("Plus", some("+"), false, metaAstNone, @[]),
  ("Minus", some("-"), false, metaAstNone, @[]),
  ("Mul", some("*"), false, metaAstNone, @[]),
  ("Div", some("/"), false, metaAstNone, @[]),
  ("Mod", some("%"), false, metaAstNone, @[]),
  ("BitAnd", some("&"), false, metaAstNone, @[]),
  ("BitOr", some("|"), false, metaAstNone, @[]),
  ("BitXor", some("^"), false, metaAstNone, @[]),
  ("BitInvert", some("~"), false, metaAstNone, @[]),
  ("LogicAnd", some("&&"), false, metaAstNone, @[]),
  ("LogicOr", some("||"), false, metaAstNone, @[]),
  ("LogicNot", some("!"), false, metaAstNone, @[]),
  ("BitShl", some("<<"), false, metaAstNone, @[]),
  ("BitShr", some(">>"), false, metaAstNone, @[]),
  #--------
  ("Assign", some("="), false, metaAstNone, @[]),
  ("AssignPlus", some("+="), false, metaAstNone, @[]),
  ("AssignMinus", some("-="), false, metaAstNone, @[]),
  ("AssignMul", some("*="), false, metaAstNone, @[]),
  ("AssignDiv", some("/="), false, metaAstNone, @[]),
  ("AssignMod", some("%="), false, metaAstNone, @[]),
  ("AssignBitAnd", some("&="), false, metaAstNone, @[]),
  ("AssignBitOr", some("|="), false, metaAstNone, @[]),
  ("AssignBitXor", some("^="), false, metaAstNone, @[]),
  ("AssignBitShl", some("<<="), false, metaAstNone, @[]),
  ("AssignBitShr", some(">>="), false, metaAstNone, @[]),
  #--------
  #--------
  # AST-only elements from here on
  (
    "ExprIdent", none(string), true, metaAstExpr,
    @[
      ("ident", astValAstNode, "Ident"),
    ]
  ),
  (
    "Unop", none(string), true, metaAstExpr,
    @[
      ("kind", astValUnopKind, ""),
      ("obj", astValAstNode, "Expr"),
    ]
  ),
  (
    "Binop", none(string), true, metaAstExpr,
    @[
      ("kind", astValBinopKind, ""),
      ("left", astValAstNode, "Expr"),
      ("right", astValAstNode, "Expr"),
    ]
  ),
  (
    "AssignEtc", none(string), true, metaAstStmt,
    @[
      ("kind", astValAssignEtcKind, ""),
      ("left", astValAstNode, "Expr"),
      ("right", astValAstNode, "Expr"),
    ]
  ),
  (
    "BasicType", none(string), true, metaAstTypeSub,
    @[
      ("kind", astValBasicTypeKind, ""),
    ],
  ),
  (
    "NamedType", none(string), true, metaAstTypeSub,
    @[
      ("ident", astValAstNode, "Ident"),
      #("genericImplSeq", astValSeqAstNode),
      ("genericImplSeq", astValSeqAstNode, "GenericArgImpl"),
    ]
  ),
  (
    "Type", none(string), true, metaAstNone,
    @[
      #("childSeq", astValSeqAstNode),
      #("varPtr", astValAstNode),
      ("kwVar", astValBool, ""),
      ("ptrDim", astValU64, ""),
      (
        "child", astValAstNode, #@["BasicType", "NamedType"]
        "TypeSub"
      ),
    ]
  ),
  (
    "FuncCall", none(string), true, metaAstExpr,
    @[
      ("ident", astValAstNode, "Ident"),
      #("genericImplSeq", astValSeqAstNode),
      #("genericImpl", astValAstNode, "GenericListImpl"),
      ("genericImplSeq", astValSeqAstNode, "GenericArgImpl"),
      ("argImplSeq", astValSeqAstNode, "FuncArgImpl"),
    ]
  ),
  (
    "FuncArgImpl", none(string), true,
      metaAstNone, # maybe change to `metaAstExpr`?
    @[
      ("ident", astValOptAstNode, "Ident"),
      ("expr", astValAstNode, "Expr"),
    ]
  ),
  (
    "GenericArgImpl", none(string), true, metaAstNone,
    @[
      ("ident", astValOptAstNode, "Ident"),
      ("type", astValAstNode, "Type"),
    ],
  ),
  (
    "StmtExprLhs", none(string), true, metaAstStmt,
    @[
      ("expr", astValAstNode, "Expr"),
    ],
  ),
  #(
  #  "StmtFuncCall", none(string), true,
  #  @[
  #    ("funcCall", astValAstNode),
  #  ],
  #),
  #(
  #  "FuncNamedArgImpl", none(string), true, metaAstNone,
  #  @[
  #    ("ident", astValAstNode, "Ident"),
  #    ("expr", astValAstNode, "Expr"),
  #  ],
  #),
  #(
  #  "GenericNamedArgImpl", none(string), true, metaAstNone,
  #  @[
  #    ("ident", astValAstNode, "Ident"),
  #    ("type", astValAstNode, "Type"),
  #  ],
  #),
  #(
  #  "GenericDeclList", none(string), true, metaAstNone,
  #  @[
  #    ("mySeq", astValSeqAstNode, "Ident")
  #  ]
  #),
  #(
  #  "FuncArgList", none(string), true,
  #  @[
  #    ("mySeq", astValSeqAstNode),
  #  ],
  #),
  (
    "VarEtcDeclMost", none(string), true, metaAstNone,
    @[
      ("ident", astValAstNode, "Ident"), # `AstIdent` index
      ("type", astValAstNode, "Type"),   # `AstType` index
    ]
  ),
  #(
  #  "MethodCall", none(string), true,
  #  @[
  #    ("obj", astValAstNode),
  #    ("funcCall", astValAstNode),
  #  ],
  #),
  #(
  #  "ParserTemp", none(string), true,
  #  @[
  #    ("mySeq", astValSeqAstNode),
  #  ],
  #),
  #("TypeUnresolved", none(string), true),
  #("TypeResolved", none(string), true),
  #("Lim", none(string)),
]

#dumpTree:
#  const `helperTokKindTest`*: seq[(string, Option[string])] = @[
#    ("tokA", some("a")),
#    ("tokB", some("b")),
#  ]



#macro `tokKindXMacro`*(): untyped = 
#  result = quote do:
#    type
#      TokKind* = enum
#
#
#  for idx in 0 ..< helperTokKindSeq.len():
#    let tempIdent = ident(helperTokKindSeq[idx][0])
#
#    result.add quote do:
#      $tempIdent,
#dumpTree:
#  type
#    TokKind* = enum
#      #--------
#      tokInternalAstStart, # fake token indicating the beginning of the AST
#      tokBad,           # invalid input!
#      #--------
#      tokIdent,         # identifiers
#      tokNumLit,        # 0-9, hex numbers, binary numbers, etc.

#StmtList
#  TypeSection
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "TokKind"
#      Empty
#      EnumTy
#        Empty
#        Ident "tokInternalAstStart"
#        Ident "tokBad"
#        Ident "tokIdent"
#        Ident "tokNumLit"

macro mkEnumTokKind(): untyped =
  var tempSeq: seq[NimNode]

  for idx in 0 ..< helperTokKindSeq.len():
    tempSeq.add ident("tok" & helperTokKindSeq[idx][0])

  result = newEnum(
    name=ident("TokKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )

mkEnumTokKind()

macro mkEnumAstKind(): untyped =
  var tempSeq: seq[NimNode]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if h[2] and h[3] == metaAstNone:
      tempSeq.add ident("ast" & helperTokKindSeq[idx][0])
  tempSeq.add ident("astStmt")
  tempSeq.add ident("astExpr")
  tempSeq.add ident("astTypeSub")

  result = newEnum(
    name=ident("AstKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )

mkEnumAstKind()

type
  MetaAstInfo* = object
    nameUpper*: string
    nameLower*: string
    kind*: Option[AstKind]

const metaAstInfoArr* = [
  MetaAstInfo(
    # metaAstNone
    nameUpper: "None",
    nameLower: "none",
    kind: none(AstKind),
  ),
  MetaAstInfo(
    # metaAstStmt
    nameUpper: "Stmt",
    nameLower: "stmt",
    kind: some(astStmt),
  ),
  MetaAstInfo(
    # metaAstExpr
    nameUpper: "Expr",
    nameLower: "expr",
    kind: some(astExpr)
  ),
  MetaAstInfo(
    # metaAstTypeSub
    nameUpper: "TypeSub",
    nameLower: "typeSub",
    kind: some(astTypeSub)
  )
]
    
type
  CurrTok* = object
    tok*: TokKind
    optStr*: Option[string]
    optU64*: Option[uint64]

proc mkCurrTok*(
  tok: TokKind,
  optStr: Option[string],
  optU64: Option[uint64],
): CurrTok = CurrTok(
  tok: tok,
  optStr: optStr,
  optU64: optU64
)

type
  LexMain* = object
    locInLine*: uint64
    lineNum*: uint64
    inpIdx*: int
    currTok*: CurrTok
      
proc locMsg*(
  lexMain: LexMain,
  inputFname: string,
  #moduleName: string,
): string =
  result = (
    (
      "at this location: " & inputFname & ":"
    ) & (
      $lexMain.lineNum & "," & $lexMain.locInLine
    )
  )
