import std/options

import ast

type
  TypeInfoMain* = object
    name*: string
    funcVar*: bool            # is this a `var`
    ptrDim*: uint           # how many `ptr`s are there in the type?
    arrDim*: uint64         # what are the array dimensions?
                            # (if any. 0 used to indicate that this
                            # *is not* an array)
    #parentSymIdx*: uint64   # the parent Symbol index
    ast*: AstNode

  TypeInfoToResolve* = object

  TypeInfoBuiltinType* = object

  TypeInfoStruct* = object
    genericIdxSeq*: seq[uint64]     # generics
    fieldIdxSeq*: seq[uint64]       # struct fields

  TypeInfoFunc* = object
    genericIdxSeq*: seq[uint64]
    argIdxSeq*: seq[uint64]     # function arguments

  TypeKind* = enum
    typeToResolve,      # this needs to be resolved in a later pass
                        # because it's a forward reference
    typeBuiltinType,
    typeStruct,
    typeFunc,
    #typeVar,
    #typeLim,
  TypeInfo* = object
    main*: TypeInfoMain
    case kind*: TypeKind
    of typeToResolve:
      myToResolve: TypeInfoToResolve
    of typeBuiltinType:
      myBuiltinType: TypeInfoBuiltinType
    of typeStruct:
      myStruct: TypeInfoStruct
    of typeFunc:
      myFunc: TypeInfoFunc

proc name*(
  self: var TypeInfo
): var string =
  self.main.name

proc ptrDim*(
  self: var TypeInfo
): var uint =
  result = self.main.ptrDim

proc arrDim*(
  self: var TypeInfo
): var uint64 =
  result = self.main.arrDim

#proc parentSymIdx*(
#  self: var TypeInfo
#): var uint64 =
#  result = self.main.parentSymIdx


type
  SymKind* = enum
    symVar,
    #symLet,
    symConst,
    symStructDecl,
    symStructGeneric,
    symStructField,
    symFuncDecl,
    symFuncGeneric,
    symFuncArg,

  Symbol* = object
    name*: string
    kind*: SymKind
    typeInfoIdx*: uint32  # surely we don't need to support 
                          # more than 1 << 32 types, right?
    initValAstIdx*: Option[uint64]  # index into the `seq[AstNode]`
                                    # indicating the initial value
    scopeIdxSeq: seq[uint64]

