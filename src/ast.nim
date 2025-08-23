import std/options
import std/macros

import nonAstDataStructures


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

#type
#  AstIdent* = object
#    


type
  AstNode* = object
    #tok*: TokKind
    #kind*: AstKind
    #lineNum*: uint64
    lexMain*: LexMain
    #u64Val*: uint64
    #strVal*: string
    #litVal*: Option[AstLitVal]
    #symIdxSeq*: seq[uint64]
    #chIdxSeq*: seq[uint64]    # indices into `Scone.ast` children
    parentIdx*: uint64
    #case kind: AstKind:
    #of astTrue:
    #case kind*: AstKind:
    #of
