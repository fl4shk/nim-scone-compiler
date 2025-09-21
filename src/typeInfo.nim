import std/options
import dataStructuresMisc

type
  TypeInfoArrKind* = enum
    tiarrNothing,
    tiarrArr,
    tiarrOpenarr,
  #TypeInfoArrOpenarr* = object
  #TypeInfoArrNothing* = object

  TypeInfoMain* = object
    #moduleName*: string
    inputFname*: string
    name*: Option[string]
    funcVar*: bool          # is this a `var` of a function signature?
    ptrDim*: uint           # how many `ptr`s are there in the type?
    arrDim*: Option[uint64]
      # what are the array dimensions, if any?
      # If the array dimensions aren't known, `arrDim` will be set to
      # `none(uint64)`
      # `arrDim` should also be set to `none(uint64)` if `arrKind` is set
      # to `tiarrNothing`
    arrKind*: TypeInfoArrKind
      # What kind of array do we have, if any?

    #case arrKind*: TypeInfoArrKind
    #of tiarrArr: arrDim*: uint64        
    #  # what are the array dimensions?
    #  # (if any. 0 used to indicate that this
    #  # *is not* an `array` (but might still be an
    #  # `openarray`))
    #of tiarrOpenarr: arrOpenarr*: TypeInfoArrOpenarr
    #of tiarrNothing: arrNothing*: TypeInfoArrNothing

    #isOpenarr*: bool        # 
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

  TypeInfoEnum* = object
    valueIdxSeq*: seq[int]

  TypeInfoVariant* = object
    genericIdxSeq*: seq[int]    # generics
    fieldIdxSeq*: seq[int]      # variant fields

  TypeInfoKind* = enum
    tiToResolve,      # this needs to be resolved in a later pass
                      # because it's a forward reference, generic, etc.
    tiBasicType,
    tiStruct,
    tiFunc,
    tiEnum,
    tiVariant,
    #tiVar,
  TypeInfo* = ref TypeInfoObj
  TypeInfoObj* = object
    main*: TypeInfoMain
    case kind*: TypeInfoKind
    of tiToResolve: myToResolve*: TypeInfoToResolve
    of tiBasicType: myBasicType*: TypeInfoBasicType
    of tiStruct: myStruct*: TypeInfoStruct
    of tiFunc: myFunc*: TypeInfoFunc
    of tiEnum: myEnum*: TypeInfoEnum
    of tiVariant: myVariant*: TypeInfoVariant

proc name*(
  self: var TypeInfo
): var Option[string] =
  self.main.name

proc funcVar*(
  self: var TypeInfo
): var bool =
  self.main.funcVar

proc ptrDim*(
  self: var TypeInfo
): var uint =
  result = self.main.ptrDim

proc arrDim*(
  self: var TypeInfo
): var Option[uint64] =
  result = self.main.arrDim

#proc isOpenarr*(
#  self: var TypeInfo
#): var bool =
#  result = self.main.isOpenarr
proc arrKind*(
  self: var TypeInfo
): var TypeInfoArrKind =
  result = self.main.arrKind
