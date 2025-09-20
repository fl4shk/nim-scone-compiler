import std/options
import dataStructuresMisc

type
  TypeInfoMain* = object
    #moduleName*: string
    inputFname*: string
    name*: Option[string]
    funcVar*: bool          # is this a `var` of a function signature?
    ptrDim*: uint           # how many `ptr`s are there in the type?
    arrDim*: uint64         # what are the array dimensions?
                            # (if any. 0 used to indicate that this
                            # *is not* an array)
    #ast*: AstNode

  TypeInfoToResolve* = object

  TypeInfoBasicType* = object
    kind*: AstBasicTypeKind

  TypeInfoStruct* = object
    # These are indices into the `SymbolTable`'s `childSeq`
    genericIdxSeq*: seq[int]     # generics
    fieldIdxSeq*: seq[int]       # struct fields

  TypeInfoFunc* = object
    # These are indices into the `SymbolTable`'s `childSeq`
    genericIdxSeq*: seq[int]
    argIdxSeq*: seq[int]     # function arguments
    resultIdx*: int

  TypeInfoKind* = enum
    tiToResolve,      # this needs to be resolved in a later pass
                      # because it's a forward reference, generic, etc.
    tiBasicType,
    tiStruct,
    tiFunc,
    #tiVar,
  TypeInfo* = ref TypeInfoObj
  TypeInfoObj* = object
    main*: TypeInfoMain
    case kind*: TypeInfoKind
    of tiToResolve: myToResolve*: TypeInfoToResolve
    of tiBasicType: myBasicType*: TypeInfoBasicType
    of tiStruct: myStruct*: TypeInfoStruct
    of tiFunc: myFunc*: TypeInfoFunc
