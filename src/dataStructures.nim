import std/options

type
  Mode* = enum
    mdOneFile,


type
  TokKind* = enum
    #--------
    tokInternalAstStart, # fake token indicating the beginning of the AST
    #--------
    tokIdent,         # identifiers
    tokNumLit,        # 0-9, hex numbers, binary numbers, etc.
    tokStrLit,        # string literals
    tokTrue,          # `true`
    tokFalse,         # `false`
    #--------
    tokLParen,        # (
    tokRParen,        # )
    tokLBracket,      # [
    tokRBracket,      # ]
    tokLBrace,        # {
    tokRBrace,        # }
    tokSemicolon,     # ;
    #--------
    tokPtr,           # `ptr`
    tokAddr,          # `addr`
    tokDot,           # `.`
    tokDeref,         # `[]` (pointer dereference)
    #--------
    tokVar,           # `var`
    tokDef,           # `def`
    #tokMacro,        # maybe save this for the bootstrapped compiler?
    tokStruct,        # `struct`
    tokEnum,          # `enum`
    tokExtern,        # `extern`
    tokCextern,       # `cextern`
    tokImport,        # `import`
    tokCImport,       # `cimport`
    #--------
    tokScope,         # `scope`
    tokIf,            # `if`
    tokElif,          # `elif`
    tokElse,          # `else`
    tokSwitch,        # `switch`,
    tokCase,          # `case`
    tokDefault,       # `default`
    tokFor,           # `for`
    tokWhile,         # `while`
    tokContinue,      # `continue`
    tokBreak,         # `break`
    tokResult,        # `result`
    #--------
    tokType,          # `type`
    tokArray,         # `array`
    tokVoid,          # `void`
    tokBool,          # `bool`
    tokU8,            # `u8`
    tokI8,            # `i8`
    tokU16,           # `u16`
    tokI16,           # `i16`
    tokU32,           # `u32`
    tokI32,           # `i32`
    tokU64,           # `u64`
    tokI64,           # `i64`
    tokF32,           # `f32`
    tokF64,           # `f64`
    tokChar,          # `char`
    tokCString,       # `cstring`
    #--------
    tokCmpEq,         # ==
    tokCmpNe,         # !=
    tokCmpLt,         # <
    tokCmpGt,         # >
    tokCmpLe,         # <=
    tokCmpGe,         # >=
    #--------
    tokPlus,          # +
    tokMinus,         # -
    tokMul,           # *
    tokDiv,           # /
    tokMod,           # %
    tokBitAnd,        # &
    tokBitOr,         # |
    tokBitXor,        # ^
    tokBitInvert,     # ~
    tokLogicAnd,      # &&
    tokLogicOr,       # ||
    tokBitShl,        # <<
    tokBitShr,        # >>
    #--------
    tokAssign,        # =
    tokAssignPlus,    # +=
    tokAssignMinus,   # -=
    tokAssignMul,     # *=
    tokAssignDiv,     # /=
    tokAssignMod,     # %=
    tokAssignBitAnd,  # &=
    tokAssignBitOr,   # |=
    tokAssignBitXor,  # ^=
    tokAssignBitShl,  # <<=
    tokAssignBitShr,  # >>=
    #--------
    tokLim,

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

