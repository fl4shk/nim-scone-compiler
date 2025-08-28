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


const `helperTokKindSeq`*: seq[(
  string, Option[string], bool,
  seq[(string, AstValKind)]
)] = @[
  #--------
  (
    # fake token indicating the beginning of the AST
    "SrcFile", none(string), true,
    @[
      ("module", astValAstNode),
      ("funcDeclSeq", astValSeqAstNode),
      ("structDeclSeq", astValSeqAstNode),
    ]
  ),
  (
    # invalid input!
    "Bad", none(string), false, @[]
  ),
  (
    # end of file
    "Eof", none(string), false, @[]
  ),
  #("LineComent", none(string)),    # start of line comment
  #--------
  (
    # identifiers
    "Ident", none(string), true,
    @[
      ("strVal", astValString)
    ]
  ),
  (
    # 0-9, hex numbers, binary numbers, etc.
    "U64Lit", none(string), true,
    @[
      ("u64Val", astValU64)
    ]
  ),
  (
    # string literals
    "StrLit", none(string), true,
    @[
      ("strLitVal", astValString)
    ]
  ),
  #--------
  ("True", some("true"), true, @[]),
  ("False", some("false"), true, @[]),
  #--------
  ("LParen", some("("), false, @[]),
  ("RParen", some(")"), false, @[]),
  ("LBracket", some("["), false, @[]),
  ("RBracket", some("]"), false, @[]),
  ("LBrace", some("{"), false, @[]),
  ("RBrace", some("}"), false, @[]),
  ("Comma", some(","), false, @[]),
  ("Semicolon", some(";"), false, @[]),
  ("Colon", some(":"), false, @[]),
  #--------
  ("Ptr", some("ptr"), true, @[]),
  (
    "Addr", some("addr"), false, # address of an expression
    @[
      #("obj", astValAstNode)
    ]
  ),
  (
    "Deref", some("@"), true, # pointer dereference
    @[
      ("obj", astValAstNode)
    ]
  ),
  (
    "Dot", some("."), true,
    @[
      ("left", astValAstNode),
      ("right", astValAstNode),
    ]
  ),
  #--------
  (
    "Var", some("var"), true,
    @[
      #("ident", astValAstNode),           # `AstIdent` index
      #("myType", astValAstNode),          # `AstType` index
      ("child", astValAstNode),           # `AstVarEtcDeclMost`
      ("optExpr", astValOptAstNode),      # optional expression index
    ]
  ),
  (
    "Const", some("const"), true,
    @[
      #("ident", astValAstNode),         # `AstIdent` index
      #("myType", astValAstNode),        # `AstType` index
      ("child", astValAstNode),         # `AstVarEtcDeclMost`
      ("expr", astValAstNode),          # expression index
    ]
  ),
  (
    "Def", some("def"), true,
    @[
      ("ident", astValAstNode),               # `AstIdent`
      ("genericDecl", astValAstNode),         # `AstGenericList`
      ("argDeclSeq", astValSeqAstNode),       # seq of `AstVarEtcDeclMost`
      ("returnType", astValAstNode),          # `AstType`
      ("stmtSeq", astValSeqAstNode),
    ],
  ),
  ("FuncReturnTypePrefix", some("->"), false, @[]),
  #("FuncNamedArgListStart", some("$(")),
  #("GenericNamedArgListStart", some("$[")),
  #("GenericArgListStart", some("#[")),
  #("Array", some("array")),
  #("Macro", some("macro")),
      # Maybe save `macro` for the bootstrapped compiler?
      # I'm not sure I outright need macros for this version of the
      # compiler
  (
    "Module", some("module"), true,
    @[
      ("ident", astValAstNode),             # `AstIdent`
    ],
  ),
  (
    "Struct", some("struct"), true,
    @[
      ("ident", astValAstNode),             # `AstIdent`
      ("genericDecl", astValAstNode),       # `AstGenericList`
      ("fieldSeq", astValSeqAstNode),       # `seq` of `AstVarEtcDeclMost`
    ],
  ),
  (
    "Enum", some("enum"), true,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Extern", some("extern"), true,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Cextern", some("cextern"), true,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "Import", some("import"), true,
    @[
      # TODO: come back to this later
    ]
  ),
  (
    "CImport", some("cimport"), true,
    @[
      # TODO: come back to this later
    ]
  ),
  #--------
  (
    "Scope", some("scope"), true,
    @[
      ("stmtSeq", astValSeqAstNode),    # the list of statements
    ]
  ),
  (
    "If", some("if"), true,
    @[
      ("expr", astValAstNode),          # (condition) expression
      ("stmtSeq", astValSeqAstNode),    # the list of statements
                                        # within the scope
      ("elifSeq", astValSeqAstNode),    # `AstElif` `seq`
      ("optElse", astValOptAstNode),    # optional `AstElse`
    ],
  ),
  (
    "Elif", some("elif"), true,
    @[
      ("expr", astValAstNode),        # (condition) expression
      ("stmtSeq", astValSeqAstNode),  # the list of statements 
                                      # within the scope
      #("optChild", astValOptAstNode), # optional `AstElif` or `AstElse`
    ],
  ),
  (
    "Else", some("else"), true,
    @[
      ("stmtSeq", astValSeqAstNode),  # the list of statements
                                      # within the scope
    ]
  ),
  (
    "Switch", some("switch"), true,
    @[
      ("expr", astValAstNode),          # the condition
      ("caseSeq", astValSeqAstNode),    # the list of `AstCase` 
      ("optDefault", astValOptAstNode), # optional `AstDefault`
    ]
  ),
  (
    "Case", some("case"), true,
    @[
      ("expr", astValAstNode),        # the condition
      ("stmtSeq", astValSeqAstNode),  # the list of statements
                                      # within the scope
    ],
  ),
  (
    "Default", some("default"), true,
    @[
      ("stmtSeq", astValSeqAstNode),  # the list of statements
                                      # within the scope
    ]
  ),
  (
    "For", some("for"), true,
    @[
      ("ident", astValAstNode),     # name of the indexing variable
      ("exprPre", astValAstNode),
      ("exprPost", astValAstNode),
      ("isUntil", astValBool),
      ("stmtSeq", astValSeqAstNode),
    ],
  ),
  ("In", some("in"), false, @[]),
  ("To", some("to"), false, @[]),
  ("Until", some("until"), false, @[]),
  (
    "While", some("while"), true,
    @[
      ("expr", astValAstNode),        # (conditional) expression
      ("stmtSeq", astValSeqAstNode),  # the list of statements
                                      # within the scope
    ],
  ),
  ("Continue", some("continue"), true, @[]),
  ("Break", some("break"), true, @[]),
  #("Result", some("result"), true),
  (
    "Return", some("return"), true,
    @[
      ("optExpr", astValOptAstNode)
    ]
  ),
  #--------
  #("Type", some("type")),
  (
    "Array", some("array"), true,
    @[
      ("dim", astValAstNode),
      ("elemType", astValAstNode),
    ]
  ),
  ("Void", some("void"), true, @[]),
  ("Bool", some("bool"), true, @[]),
  ("U8", some("u8"), true, @[]),
  ("I8", some("i8"), true, @[]),
  ("U16", some("u16"), true, @[]),
  ("I16", some("i16"), true, @[]),
  ("U32", some("u32"), true, @[]),
  ("I32", some("i32"), true, @[]),
  ("U64", some("u64"), true, @[]),
  ("I64", some("i64"), true, @[]),
  ("F32", some("f32"), true, @[]),
  ("F64", some("f64"), true, @[]),
  ("Char", some("char"), true, @[]),
  ("String", some("string"), true, @[]),
  #--------
  ("CmpEq", some("=="), false, @[]),
  ("CmpNe", some("!="), false, @[]),
  ("CmpLt", some("<"), false, @[]),
  ("CmpGt", some(">"), false, @[]),
  ("CmpLe", some("<="), false, @[]),
  ("CmpGe", some(">="), false, @[]),
  #--------
  ("Plus", some("+"), false, @[]),
  ("Minus", some("-"), false, @[]),
  ("Mul", some("*"), false, @[]),
  ("Div", some("/"), false, @[]),
  ("Mod", some("%"), false, @[]),
  ("BitAnd", some("&"), false, @[]),
  ("BitOr", some("|"), false, @[]),
  ("BitXor", some("^"), false, @[]),
  ("BitInvert", some("~"), false, @[]),
  ("LogicAnd", some("&&"), false, @[]),
  ("LogicOr", some("||"), false, @[]),
  ("LogicNot", some("!"), false, @[]),
  ("BitShl", some("<<"), false, @[]),
  ("BitShr", some(">>"), false, @[]),
  #--------
  ("Assign", some("="), false, @[]),
  ("AssignPlus", some("+="), false, @[]),
  ("AssignMinus", some("-="), false, @[]),
  ("AssignMul", some("*="), false, @[]),
  ("AssignDiv", some("/="), false, @[]),
  ("AssignMod", some("%="), false, @[]),
  ("AssignBitAnd", some("&="), false, @[]),
  ("AssignBitOr", some("|="), false, @[]),
  ("AssignBitXor", some("^="), false, @[]),
  ("AssignBitShl", some("<<="), false, @[]),
  ("AssignBitShr", some(">>="), false, @[]),
  #--------
  #--------
  # AST-only elements from here on
  (
    "Unop", none(string), true,
    @[
      ("kind", astValUnopKind),
      ("obj", astValAstNode),
    ]
  ),
  (
    "Binop", none(string), true,
    @[
      ("kind", astValBinopKind),
      ("left", astValAstNode),
      ("right", astValAstNode),
    ]
  ),
  (
    "AssignEtc", none(string), true,
    @[
      ("kind", astValAssignEtcKind),
      ("left", astValAstNode),
      ("right", astValAstNode),
    ]
  ),
  (
    "NamedType", none(string), true,
    @[
      ("ident", astValAstNode),
      #("genericImplSeq", astValSeqAstNode),
      ("genericImpl", astValAstNode),
    ]
  ),
  (
    "Type", none(string), true,
    @[
      #("childSeq", astValSeqAstNode),
      #("varPtr", astValAstNode),
      ("kwVar", astValBool),
      ("ptrDim", astValU64),
      ("child", astValAstNode),
    ]
  ),
  (
    "FuncCall", none(string), true,
    @[
      ("ident", astValAstNode),
      #("genericImplSeq", astValSeqAstNode),
      ("genericImpl", astValAstNode),
      ("argImplSeq", astValSeqAstNode),
    ]
  ),
  (
    "FuncNamedArgImpl", none(string), true,
    @[
      ("ident", astValAstNode),
      ("expr", astValAstNode),
    ],
  ),
  (
    "GenericNamedArgImpl", none(string), true,
    @[
      ("ident", astValAstNode),
      ("type", astValAstNode),
    ],
  ),
  (
    "GenericList", none(string), true,
    @[
      ("mySeq", astValSeqAstNode)
    ]
  ),
  (
    "VarEtcDeclMost", none(string), true,
    @[
      ("ident", astValAstNode),           # `AstIdent` index
      ("type", astValAstNode),          # `AstType` index
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
    if helperTokKindSeq[idx][2]:
      tempSeq.add ident("ast" & helperTokKindSeq[idx][0])

  result = newEnum(
    name=ident("AstKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )

mkEnumAstKind()


type
  TypeInfoMain* = object
    name*: string
    ptrDim*: uint           # how many `ptr`s are there in the type?
    arrDim*: uint64         # what are the array dimensions (if any)?
    parentSymIdx*: uint64   # the parent symbol

type
  TypeInfoUnknown* = object

type
  TypeInfoBuiltinType* = object

type
  TypeInfoStruct* = object
    chIdxSeq*: seq[uint64]  # indices into the `TypeInfo` table
                            # indicating struct fields, the next array
                            # dimension, etc.

type
  TypeInfoFunc* = object
    argIdxSeq*: seq[uint64]     # function arguments

type
  TypeKind* = enum
    typeKindUnknown,      # this needs to be resolved in a later pass
                          # because it's a forward reference
    typeKindBuiltinType,
    typeKindStruct,
    typeKindFunc,
    #typeKindVar,
    #typeKindLim,
  TypeInfo* = object
    main*: TypeInfoMain
    case kind*: TypeKind
    of typeKindUnknown:
      tiUnkVal: TypeInfoUnknown
    of typeKindBuiltinType:
      tiBuiltinTypeVal: TypeInfoBuiltinType
    of typeKindStruct:
      tiStructVal: TypeInfoStruct
    of typeKindFunc:
      tiFuncVal: TypeInfoFunc

proc name*(
  self: var TypeInfo
): var string =
  self.main.name
  #case self.kind:
  #of typeKindUnknown:
  #  return self.tiUnkVal.main.name
  #of typeKindBuiltinType:
  #  return self.tiBuiltinTypeVal.main.name
  #of typeKindStruct:
  #  return self.tiStructVal.main.name
  #of typeKindFunc:
  #  return self.tiFuncVal.main.name

proc ptrDim*(
  self: var TypeInfo
): var uint =
  result = self.main.ptrDim
  #case self.kind:
  #of typeKindUnknown:
  #  return self.tiUnkVal.main.ptrDim
  #of typeKindBuiltinType:
  #  return self.tiBuiltinTypeVal.main.ptrDim
  #of typeKindStruct:
  #  return self.tiStructVal.main.ptrDim
  #of typeKindFunc:
  #  return self.tiFuncVal.main.ptrDim

proc arrDim*(
  self: var TypeInfo
): var uint64 =
  result = self.main.arrDim
  #case self.kind:
  #of typeKindUnknown:
  #  return self.tiUnkVal.main.arrDim
  #of typeKindBuiltinType:
  #  return self.tiBuiltinTypeVal.main.arrDim
  #of typeKindStruct:
  #  return self.tiStructVal.main.arrDim
  #of typeKindFunc:
  #  return self.tiFuncVal.main.arrDim

proc parentSymIdx*(
  self: var TypeInfo
): var uint64 =
  result = self.main.parentSymIdx
  #case self.kind:
  #of typeKindUnknown:
  #  return self.tiUnkVal.main.parentSymIdx
  #of typeKindBuiltinType:
  #  return self.tiBuiltinTypeVal.main.parentSymIdx
  #of typeKindStruct:
  #  return self.tiStructVal.main.parentSymIdx
  #of typeKindFunc:
  #  return self.tiFuncVal.main.parentSymIdx


type
  SymKind* = enum
    symKindVar,
    symKindLet,
    symKindConst,
  Symbol* = object
    name*: string
    kind*: SymKind
    typeInfoIdx*: uint  # surely we don't need to support 
                        # more than 1 << 32 types, right?
    initValAstIdx*: Option[uint64]  # index into the `seq[AstNode]`
                                    # indicating the initial value
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
      
