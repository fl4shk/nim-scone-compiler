import std/options
import std/tables
import std/sets

import dataStructuresMisc
import ast
import reduceEtc
import typeInfo

type
  SymKind* = enum
    #symModule,
    symVar,
    #symLet,
    symConst,
    symGenericDecl,
    symGenericImpl,
    symStructDecl,
    symStructDeclField,
    symStructImpl,
    symStructImplField,
    #symVariantDecl,
    #symVariantField,
    symFuncDecl,
    # #symFuncDeclArg,
    # #symFuncDeclReturnType,

    symFuncCall,
    symFuncCallArg,
    # #symMacroDecl,
    # #symMacroArg
    # #symMacroReturnType,

  Symbol* = ref SymbolObj
  SymbolObj* = object
    #moduleName*: string
    inputFname*: string
    name*: string
    kind*: SymKind
    initValAst*: Option[AstNode]  # the `AstNode` indicating the initial
                                  # value/`const` value
    typeInfo*: TypeInfo
    #parent*: SymbolTable

  SymbolTable* = ref SymbolTableObj
  SymbolTableObj* = object
    ast*: AstNode                 # the `AstNode` of the scope
    sym*: Option[Symbol]          # The `Symbol` this `SymbolTable`
                                  # represents
    nameTbl*: OrderedTable[string, seq[int]]
                                  # mapping from symbol name
                                  # to indices into `childSeq`
    parent*: SymbolTable          # the parent `SymbolTable` of this one
    childSeq*: seq[SymbolTable]   # the children `SymbolTable`s of this one

    #decl*: Option[SymbolTable]    # The `SymbolTable` of the declaration
    #                              # that is implemented by this
    #                              # `SymbolTable`
    #                              # (for example, the specific function
    #                              # that is overloaded by this instance)

    # These `OrderedTable`s map names to 
    #structNameTbl*: OrderedTable[string, int]
    #funcNameTbl*: OrderedTable[string, seq[int]]
    #varNameTbl*: OrderedTable[string, seq[int]]
proc isNamed*(
  self: Symbol
): bool =
  result = (self.name.len() > 0)

#proc parentSymIdx*(
#  self: var TypeInfo
#): var uint64 =
#  result = self.main.parentSymIdx


#proc findSymSeq*(
#  self: SymbolTable,
#  name: string,
#): seq[Symbol] =
#  #result = none(seq[Symbol])

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

  var foundIdxSeq: HashSet[int]
  if self.sym.isSome: #!= nil:
    result.add i & "named: \"" & self.sym.get().name & "\"\n"
    result.add i & "* " & "symbol: " & $self.sym.get()[] & "\n"
    result.add i & "* " & "typeInfo: " & $self.sym.get().typeInfo[] & "\n"
    result.add "\n"
    for name, idxSeq in self.nameTbl:
      foundIdxSeq = foundIdxSeq.union(toHashSet(idxSeq))
      for idx in idxSeq:
        let child = self.childSeq[idx]
        result.add child.toStr(x)
  #result.add "----\n"
  if foundIdxSeq.len() < self.childSeq.len():
    result.add i & "unnamed:" & "\n"
  for idx in 0 ..< self.childSeq.len():
    if idx in foundIdxSeq:
      #echo "idx found: " & $idx
      continue
    #else:
    #  #echo "idx not found: " & $idx
    #  discard
    let child = self.childSeq[idx]
    result.add sconcat(@[
      child.toStr(x)
    ])
  if foundIdxSeq.len() < self.childSeq.len():
    #result.add i & "unnamed:" & "\n"
    result.add i & "--------\n"
  #result.add "--------\n\n"
