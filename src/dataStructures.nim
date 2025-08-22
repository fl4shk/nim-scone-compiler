import std/options
import std/macros

type
  Mode* = enum
    mdOneFile,

const `helperTokKindSeq`*: seq[(string, Option[string])] = @[
  #--------
  ("tokInternalAstStart", none(string)), # fake token indicating the beginning of the AST
  ("tokBad", none(string)),           # invalid input!
  ("tokEof", none(string)),           # end of file
  #("tokLineComent", none(string)),    # start of line comment
  #--------
  ("tokIdent", none(string)),  # identifiers
  ("tokU64Lit", none(string)), # 0-9, hex numbers, binary numbers, etc.
  ("tokStrLit", none(string)), # string literals
  #--------
  ("tokTrue", some("true")),
  ("tokFalse", some("false")),
  #--------
  ("tokLParen", some("(")),
  ("tokRParen", some(")")),
  ("tokLBracket", some("[")),
  ("tokRBracket", some("]")),
  ("tokLBrace", some("{")),
  ("tokRBrace", some("}")),
  ("tokComma", some(",")),
  ("tokSemicolon", some(";")),
  ("tokColon", some(":")),
  #--------
  ("tokPtr", some("ptr")),
  ("tokAddr", some("addr")),
  ("tokDeref", some("[]")), # pointer dereference
  ("tokDot", some(".")),
  #--------
  ("tokVar", some("var")),
  ("tokConst", some("const")),
  ("tokDef", some("def")),
  ("tokFuncReturnTypePrefix", some("->")),
  #("tokFuncNamedArgListStart", some("$(")),
  #("tokGenericNamedArgListStart", some("$[")),
  ("tokArray", some("array")),
  #("tokMacro", some("macro")),
      # Maybe save `macro` for the bootstrapped compiler?
      # I'm not sure I outright need macros for this version of the
      # compiler
  ("tokModule", some("module")),
  ("tokStruct", some("struct")),
  ("tokEnum", some("enum")),
  ("tokExtern", some("extern")),
  ("tokCextern", some("cextern")),
  ("tokImport", some("import")),
  ("tokCImport", some("cimport")),
  #--------
  ("tokScope", some("scope")),
  ("tokIf", some("if")),
  ("tokElif", some("elif")),
  ("tokElse", some("else")),
  ("tokSwitch", some("switch,")),
  ("tokCase", some("case")),
  ("tokDefault", some("default")),
  ("tokFor", some("for")),
  ("tokWhile", some("while")),
  ("tokContinue", some("continue")),
  ("tokBreak", some("break")),
  ("tokResult", some("result")),
  #--------
  ("tokType", some("type")),
  #("tokArray", some("array")),
  ("tokVoid", some("void")),
  ("tokBool", some("bool")),
  ("tokU8", some("u8")),
  ("tokI8", some("i8")),
  ("tokU16", some("u16")),
  ("tokI16", some("i16")),
  ("tokU32", some("u32")),
  ("tokI32", some("i32")),
  ("tokU64", some("u64")),
  ("tokI64", some("i64")),
  ("tokF32", some("f32")),
  ("tokF64", some("f64")),
  ("tokChar", some("char")),
  ("tokString", some("string")),
  #--------
  ("tokCmpEq", some("==")),
  ("tokCmpNe", some("!=")),
  ("tokCmpLt", some("<")),
  ("tokCmpGt", some(">")),
  ("tokCmpLe", some("<=")),
  ("tokCmpGe", some(">=")),
  #--------
  ("tokPlus", some("+")),
  ("tokMinus", some("-")),
  ("tokMul", some("*")),
  ("tokDiv", some("/")),
  ("tokMod", some("%")),
  ("tokBitAnd", some("&")),
  ("tokBitOr", some("|")),
  ("tokBitXor", some("^")),
  ("tokBitInvert", some("~")),
  ("tokLogicAnd", some("&&")),
  ("tokLogicOr", some("||")),
  ("tokLogicNot", some("!")),
  ("tokBitShl", some("<<")),
  ("tokBitShr", some(">>")),
  #--------
  ("tokAssign", some("=")),
  ("tokAssignPlus", some("+=")),
  ("tokAssignMinus", some("-=")),
  ("tokAssignMul", some("*=")),
  ("tokAssignDiv", some("/=")),
  ("tokAssignMod", some("%=")),
  ("tokAssignBitAnd", some("&=")),
  ("tokAssignBitOr", some("|=")),
  ("tokAssignBitXor", some("^=")),
  ("tokAssignBitShl", some("<<=")),
  ("tokAssignBitShr", some(">>=")),
  #--------
  #("tokLim", none(string)),
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
    tempSeq.add ident(helperTokKindSeq[idx][0])
    
  result = newEnum(
    name=ident("TokKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )

mkEnumTokKind()


#let temp = tokAssignDiv


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
  AstLitValKind* = enum
    astLvKindI64,
    astLvKindU64,
    #astLvKindF32,
    #astLvKindF64,
    astLvKindStr,

  AstLitVal* = object
    case kind: AstLitValKind
    of astLvKindI64:
      i64Val: int64
    of astLvKindU64:
      u64Val: uint64
    #of astLvKindF32:
    #  f32Val: string
    #of astLvKindF64:
    #  f64Val: string
    of astLvKindStr:
      strVal: string

type
  AstNode* = object
    tok*: TokKind
    lineNum*: uint64
    #u64Val*: uint64
    #strVal*: string
    litVal*: Option[AstLitVal]
    symIdxSeq*: seq[uint64]
    chIdxSeq*: seq[uint64]    # indices into `Scone.ast` children
    parentIdx*: uint64
