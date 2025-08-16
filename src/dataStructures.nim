import std/options
import std/macros

type
  Mode* = enum
    mdOneFile,

const `helperTokKind`*: seq[(string, Option[string])] = @[
  #--------
  ("tokInternalAstStart", none(string)), # fake token indicating the beginning of the AST
  ("tokBad", none(string)),           # invalid input!
  #--------
  ("tokIdent", none(string)),  # identifiers
  ("tokNumLit", none(string)), # 0-9, hex numbers, binary numbers, etc.
  ("tokStrLit", none(string)), # string literals
  ("tokTrue", some("true")),
  ("tokFalse", some("false")),
  #--------
  ("tokLParen", some("(")),
  ("tokRParen", some(")")),
  ("tokLBracket", some("[")),
  ("tokRBracket", some("]")),
  ("tokLBrace", some("{")),
  ("tokRBrace", some("}")),
  ("tokSemicolon", some(";")),
  #--------
  ("tokPtr", some("ptr")),
  ("tokAddr", some("addr")),
  ("tokDot", some(".")),
  ("tokDeref", some("[] ")), # pointer dereference
  #--------
  ("tokVar", some("var")),
  ("tokDef", some("def")),
  #("tokMacro", some("macro")),
      # Maybe save `macro` for the bootstrapped compiler?
      # I'm not sure I outright need macros for this version of the
      # compiler
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
  ("tokArray", some("array")),
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
  ("tokCString", some("cstring")),
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
  ("tokLim", none(string)),
]

#macro `tokKindXMacro`*(): untyped = 
#  result = quote do:
#    type
#      TokKind* = enum
#
#
#  for idx in 0 ..< helperTokKind.len():
#    let tempIdent = ident(helperTokKind[idx][0])
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

  for idx in 0 ..< helperTokKind.len():
    tempSeq.add ident(helperTokKind[idx][0])
    
  result = newEnum(
    name=ident("TokKind"),
    fields=tempSeq,
    public=true,
    pure=false,
  )

mkEnumTokKind()

type
  TypeKind* = enum
    typeKindUnknown,      # this needs to be resolved because it's a
                          # forward reference
    typeKindBuiltinType,
    typeKindStruct,
    typeKindFunc,
    typeKindVar,
    typeKindLim,

type
  TypeInfo* = object
    # covers both the type of a function 
    name*: string
    kind*: TypeKind
    ptrDim*: uint               # how many `ptr`s are there in the type?
    arrDimSeq*: seq[uint64]     # what are the array dimensions?
    fieldIdxSeq*: seq[uint64]   # indices into the `TypeInfo` table
                                # indicating struct fields, etc.
    argIdxSeq*: seq[uint64]     # function arguments
    parentSymIdx*: uint64

type
  Symbol* = object
    name*: string
    typeInfo*: TypeInfo
    initValAstIdx*: Option[uint64]  # index into the `seq[AstNode]`
                                    # indicating the initial value

type
  AstNode* = object
    tok*: TokKind
    lineNum*: uint64
    strData*: string
    chIdxSeq*: seq[uint64]    # indices into `Scone.ast` children
    parentIdx*: uint64

