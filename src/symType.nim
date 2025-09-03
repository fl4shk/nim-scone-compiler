import std/options
import std/tables
import std/sets

import dataStructuresMisc
import ast

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
    #parentSymIdx*: uint64   # the parent Symbol index
    ast*: AstNode

  TypeInfoToResolve* = object

  TypeInfoBasicType* = object
    kind*: AstBasicTypeKind

  TypeInfoStruct* = object
    genericIdxSeq*: seq[uint64]     # generics
    fieldIdxSeq*: seq[uint64]       # struct fields

  TypeInfoFunc* = object
    genericIdxSeq*: seq[uint64]
    argIdxSeq*: seq[uint64]     # function arguments
    returnType*: uint64

  TypeInfoKind* = enum
    tiToResolve,      # this needs to be resolved in a later pass
                        # because it's a forward reference, generic, etc.
    tiBasicType,
    tiStruct,
    tiFunc,
    #typeVar,
    #typeLim,
  TypeInfo* = ref TypeInfoObj
  TypeInfoObj* = object
    main*: TypeInfoMain
    case kind*: TypeInfoKind
    of tiToResolve: myToResolve*: TypeInfoToResolve
    of tiBasicType: myBasicType*: TypeInfoBasicType
    of tiStruct: myStruct*: TypeInfoStruct
    of tiFunc: myFunc*: TypeInfoFunc

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
): var uint64 =
  result = self.main.arrDim

#proc parentSymIdx*(
#  self: var TypeInfo
#): var uint64 =
#  result = self.main.parentSymIdx


type
  SymKind* = enum
    #symModule,
    symVar,
    #symLet,
    symConst,
    symGeneric,
    symStructDecl,
    #symStructGeneric,
    symStructField,
    symFuncDecl,
    #symFuncGeneric,
    symFuncArg,
    symFuncReturnType,

  Symbol* = ref SymbolObj
  SymbolObj* = object
    inputFname*: string
    #inputFname*: string
    name*: string
    kind*: SymKind
    #typeInfoIdx*: uint32          # surely we don't need to support 
    #                              # more than 1 << 32 types, right?
    initValAst*: Option[AstNode]  # index into the `seq[AstNode]`
                                  # indicating the initial value
    typeInfo*: TypeInfo
    #chIdxSeq: seq[uint64]

  SymbolTable* = ref SymbolTableObj
  SymbolTableObj* = object
    scopeAst*: AstNode            # the `AstNode` of the scope
    #symSeq*: seq[Symbol]          # the `Symbol`s themselves
    #typeInfoSeq*: seq[TypeInfo]   # the `TypeInfo`s themselves
    sym*: Option[Symbol]          # The `Symbol` this `SymbolTable`
                                  # represents
    #typeInfo*: TypeInfo           # The `TypeInfo` of `sym`
    tbl*: Table[string, seq[int]] # mapping from symbol name
                                  # to indices into `childSeq`
    parent*: SymbolTable          # the parent `SymbolTable` of this one
    childSeq*: seq[SymbolTable]   # the children `SymbolTable`s of this one

#proc findSymRev*(
#  self: SymbolTable,
#  name: string,
#): seq[Symbol] =
#  result = none(seq[Symbol])

proc toStr*(
  self: SymbolTable,
  indent: uint=0,
): string =
  let x = indent + 2
  let i = doIndent(indent=indent)

  #result.add "self.name: " & self.name
  #resuld.add "moduleName: " & self.symSeq[0].

  #echo "testificate: " & $self.childSeq.len()
  #echo "further test: " & $self.tbl

  #var foundIdx
  var foundIdxSeq: HashSet[int]
  #echo "self.sym.isSome: " & $self.sym.isSome
  if self.sym.isSome: #!= nil:
    result.add i & "symbol: " & $self.sym.get()[] & "\n"
    #result.add(
    #  i & "typeInfo: " & $self.typeInfoSeq[sym[].typeInfoIdx] & "\n"
    #)
    #result.add(
    #  i & "typeInfo: " & $self.typeInfo[] & "\n"
    #)
    result.add "\n"
    for name, idxSeq in self.tbl:
      foundIdxSeq = foundIdxSeq.union(toHashSet(idxSeq))
      for idx in idxSeq:
        let child = self.childSeq[idx]
        result.add child.toStr(x)
        #let sym = child.sym
        #let typeInfo = child.typeInfo
        #result.add i & "symbol: " & $sym[] & "\n"
        ##result.add(
        ##  i & "typeInfo: " & $self.typeInfoSeq[sym[].typeInfoIdx] & "\n"
        ##)
        #result.add(
        #  i & "typeInfo: " & $typeInfo & "\n"
        #)
        #result.add "\n"
  result.add "----\n"
  for idx in 0 ..< self.childSeq.len():
    if idx in foundIdxSeq:
      #echo "idx found: " & $idx
      continue
    else:
      #echo "idx not found: " & $idx
      discard
    let child = self.childSeq[idx]
    result.add child.toStr(x)
  result.add "--------\n\n"
  #result.add i & "#---- ----\n"
  #for childSymTbl in self.childSeq:
  #  result.add childSymTbl.toStr(x)
  #result.add "\n\n"
