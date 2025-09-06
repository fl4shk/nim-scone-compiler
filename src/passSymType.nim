import std/tables
import std/sets
import std/options

import scone
import dataStructuresMisc
import ast
import symType

type
  SconeSubPassSymType = enum
    #spstMkSymbolTables,
    #spstHandleImport,
    spstFindTopLevelDecls,
    spstSubstGenerics,
    spstHandleFuncOverloading,
    spstTypeCheck,
    limSpSymType,

  SymTypeArgs = object
    self*: ptr Scone
    changed: bool
    subPass*: SconeSubPassSymType
    ast*: AstNode
    parentAstSeq*: ptr seq[AstNode]
    parentSymSeq*: ptr seq[Symbol]
    #parentResultSeq*: ptr seq[SymTypeResult]
    moduleIdent*: ptr string
    #chkSeq*: ptr seq[bool]
    #chk*: bool

  SymTypeResultKind = enum
    stResultNone,
    stResultIdent,
    stResultU64,
    stResultString,
    stResultBool,
    stResultExpr,
    stResultSym,
    stResultType,
    stResultSeq,

  SymTypeResult = ref SymTypeResultObj
  SymTypeResultObj = object
    #isScope*: bool
    scopeAst*: AstNode
    case kind*: SymTypeResultKind
    of stResultNone: myResultNone*: uint8
    of stResultIdent: myIdent*: string
    of stResultString: myString*: string
    of stResultU64: myU64*: uint64
    of stResultBool: myBool*: bool
    of stResultExpr: myExpr*: AstNode
    of stResultSym: mySym*: Symbol
    of stResultType: myType*: TypeInfo
    of stResultSeq: mySeq*: seq[SymTypeResult]

template eek(): untyped =
  doAssert(
    false,
    "eek! " & $args.subPass & " " & $args.ast
  )
template parentEek(): untyped =
  doAssert(
    false,
    "eek! " & $parentSym[].kind & " " & $args.parentAstSeq[][^1]
  )
#template getParentSym(): untyped =
#  addr self[].symS2d[^1][args.parentSymIdxSeq[][^1]]
  

template myDoIt(
  childAst: AstNode,
  someParentSym: Option[Symbol],
): untyped =
  args.parentAstSeq[].add myAst
  if someParentSym.isSome:
    args.parentSymSeq[].add someParentSym.get()

  var hiddenTempArgs = SymTypeArgs(
    self: args.self,
    subPass: args.subPass,
    ast: childAst,
    parentAstSeq: args.parentAstSeq,
    parentSymSeq: args.parentSymSeq,
    moduleIdent: args.moduleIdent
  )
  var hiddenTempResult = hiddenTempArgs.doPassSymTypeMain()
  discard args.parentAstSeq[].pop()
  if someParentSym.isSome:
    discard args.parentSymSeq[].pop()
  hiddenTempResult

template myAst(): untyped =
  args.ast
#template typeInfo(
#  sym: Symbol
#): untyped =
#  self[].typeInfoS2d[^1][sym.typeInfoIdx]

proc doPassSymTypeMain(
  args: var SymTypeArgs
): SymTypeResult

#template mkSymbolTable(): untyped =
#  #var hiddenSymTbl = SymbolTable(
#  #  scopeAst: myAst,
#  #)
#  #self[].addChildSymTbl(hiddenSymTbl)
#  #hiddenSymTbl
#  #result.isScope =
#  discard args.self[].mkSymbolTableMain(scopeAst=myAst)
#  defer: args.self[].gotoParentSymTbl()

template mkScopeEtc(
  argSym: Option[Symbol],
  argScopeAst: AstNode,
): untyped =
  args.self[].addSym(
    sym=argSym,
    scopeAst=argScopeAst,
  )
  defer:
    if argSym.isSome:
      if not args.self[].inFindDecls:
        args.self[].checkDuplSym()
    args.self[].gotoParentSymTbl()

proc doAstSrcFile(
  args: var SymTypeArgs
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #mkSymbolTable()
  let self = args.self
  self[].inFindDecls = (
    args.subPass == spstFindTopLevelDecls
  )

  mkScopeEtc(none(Symbol), myAst)
  #var mySymTbl = SymbolTable(
  #  scopeAst: 
  #)
  #self[].nextSymTblPass()
  discard myAst.mySrcFile.module.myDoIt(none(Symbol))
  case args.subPass:
  of spstFindTopLevelDecls:
    args.changed = true
    while args.changed:
      args.changed = false
      for structDecl in myAst.mySrcFile.structDeclSeq:
        discard structDecl.myDoIt(none(Symbol))
  else:
    discard
  for funcDecl in myAst.mySrcFile.funcDeclSeq:
    discard funcDecl.myDoIt(none(Symbol))

proc doAstIdent(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultIdent,
    myIdent: myAst.myIdent.strVal,
  )
proc doAstU64Lit(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultU64,
    myU64: myAst.myU64Lit.u64Val,
  )
proc doAstStrLit(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultString,
    myString: myAst.myStrLit.strLitVal,
  )

proc doAstTrue(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultBool,
    myBool: true,
  )
proc doAstFalse(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultBool,
    myBool: false,
  )

proc doAstDeref(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
proc doAstDot(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
proc doAstVar(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
proc doAstConst(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
proc doAstDef(
  args: var SymTypeArgs,
): SymTypeResult =
  #mkSymbolTable()
  let self = args.self
  let ident = myAst.myDef.ident.myDoIt(none(Symbol)).myIdent

  case args.subPass:
  #of spstMkSymbolTables:
  #  discard
  of spstFindTopLevelDecls:
    var sym = Symbol(
      #moduleName: args.moduleIdent[],
      inputFname: self[].inputFname,
      name: ident,
      kind: symFuncDecl,
      #typeInfoIdx: uint32(self[].typeInfoS2d[^1].len()),
      #initValAstIdx: none(uint64) # function declarations don't have
                                  # initial values
    )
    sym.typeInfo = TypeInfo(
      main: TypeInfoMain(
        #moduleName: args.moduleIdent[],
        inputFname: self[].inputFname,
        name: none(string), # Functions themselves are not a named types 
                            # There may be function pointers
                            # implemented in this version of the compiler
                            # at a later date.
        funcVar: false,
        ptrDim: 0,
        arrDim: 0,
      ),
      kind: tiFunc,
    )
    mkScopeEtc(some(sym), myAst)
    discard myAst.myDef.genericDecl.myDoIt(some(sym))
    block:
      for idx in 0 ..< myAst.myDef.argDeclSeq.len():
        var mySym = myAst.myDef.argDeclSeq[idx].myDoIt(some(sym))
        mkScopeEtc(some(mySym.mySym), mySym.scopeAst)
    block:
      var resultSym = Symbol(
        inputFname: self[].inputFname,
        name: "<result>",
        kind: symVar,
      )
      resultSym.typeInfo = myAst.myDef.returnType.myDoIt(some(sym)).myType
      mkScopeEtc(some(resultSym), myAst.myDef.returnType)
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()
proc doAstModule(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #assert(
  #  args.moduleIdent.len() == 0,
  #  "eek! moduleIdent:" & args.moduleIdent & ": " & $myAst
  #)
  args.moduleIdent[] = (
    myAst.myModule.ident.myDoIt(none(Symbol)).myIdent
  )
  #echo "test: " & args.moduleIdent
proc doAstStruct(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
  #if args.self.symS2d
proc doAstEnum(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstExtern(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstCextern(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstImport(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstCImport(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstScope(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstIf(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstElif(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstElse(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstSwitch(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstCase(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstDefault(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstFor(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstWhile(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstContinue(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstBreak(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstReturn(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstArray(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    result = myAst.myArray.elemType.myDoIt(none(Symbol))
    let tempArrDim = myAst.myArray.dim.constEval(self[].inputFname)
    if tempArrDim <= 0i64:
      doAssert(
        false,
        (
          "Error: evaluated array dimension invalid "
        ) & (
          "(because " & $tempArrDim & " <= 0): ("
        ) & (
          myAst.lexMain.locMsg(self[].inputFname)
        ) & (
          ")"
        )
      )
    result.myType.arrDim() = uint64(tempArrDim)
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()
proc doAstUnop(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstBinop(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstAssignEtc(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstBasicType(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    result = SymTypeResult(
      scopeAst: myAst,
      kind: stResultType
    )
    result.myType = TypeInfo(
      main: TypeInfoMain(
        #moduleName: args.moduleIdent[],
        inputFname: self[].inputFname,
        name: none(string),
        funcVar: false,
        ptrDim: 0,
        arrDim: 0,
        ast: myAst
      ),
      kind: tiBasicType,
    )
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()

proc doAstNamedType(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    #result = SymTypeResult(
    #  kind: stResultType,
    #  myType: myAst.myType.child.myDoIt(none(Symbol)),
    #)
    let ident = myAst.myNamedType.ident.myDoIt(none(Symbol)).myIdent
    result = SymTypeResult(
      scopeAst: myAst,
      kind: stResultType,
    )

    result.myType = TypeInfo(
      main: TypeInfoMain(
        #moduleName: args.moduleIdent[],
        inputFname: self[].inputFname,
        name: some(ident),
        funcVar: false,
        ptrDim: 0,
        arrDim: 0,
        ast: myAst,
      ),
      kind: tiToResolve,
    )
    #result = myAst.myNamedType.child.myDoIt(none(Symbol))
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()

proc doAstType(
  args: var SymTypeArgs,
): SymTypeResult =
  #result = SymTypeResult(kind: stResultType)
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  case args.subPass:
  of spstFindTopLevelDecls:
    #result = SymTypeResult(
    #  kind: stResultType,
    #  myType: myAst.myType.child.myDoIt(none(Symbol)),
    #)
    result = myAst.myType.child.myDoIt(none(Symbol))
    #echo $result.kind
    result.myType.funcVar() = false
    result.myType.ptrDim() = 0u
    if myAst.myType.kwVar:
      result.myType.funcVar() = true
    elif myAst.myType.ptrDim > 0:
      result.myType.ptrDim() = myAst.myType.ptrDim
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()
  #let self = args.self
proc doAstFuncCall(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstStmtExprLhs(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstFuncNamedArgImpl(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstGenericNamedArgImpl(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstGenericList(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  let self = args.self
  for idx in 0 ..< myAst.myGenericList.mySeq.len():
    #--------
    let genericItem = addr myAst.myGenericList.mySeq[idx]
    #let parentSym = getParentSym()
    let parentSym = args.parentSymSeq[][^1]
    #--------
    case args.subPass:
    of spstFindTopLevelDecls:
      # We don't support anything but basic generics for the Nim
      # implementation of the Scone language.
      let identResult = genericItem[].myDoIt(none(Symbol))#.myIdent
      let ident = identResult.myIdent
      var sym = Symbol(
        #moduleName: args.moduleIdent[],
        inputFname: self[].inputFname,
        name: ident,
        initValAst: none(AstNode),
        #initValAstIdx: none(uint64)
      )
      sym.typeInfo = TypeInfo(
        main: TypeInfoMain(
          #moduleName: args.moduleIdent[],
          inputFname: self[].inputFname,
          name: none(string),
          funcVar: false,
          ptrDim: 0,
          arrDim: 0,
        ),
        kind: tiToResolve
      )
      case parentSym[].kind:
      of symStructDecl, symFuncDecl:
        sym.kind = symGeneric #symStructGeneric
      #of symFuncDecl:
      #  sym.kind = symGeneric #symFuncGeneric
      else:
        parentEek()
      mkScopeEtc(some(sym), identResult.scopeAst)
      #self[].addSym(some(sym))
    of spstSubstGenerics:
      discard
    of spstHandleFuncOverloading:
      discard
    of spstTypeCheck:
      discard
    else:
      #doAssert(
      #  false,
      #  "eek! " & $args.subPass
      #)
      eek()
    #--------

proc doAstVarEtcDeclMost(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    scopeAst: myAst,
    kind: stResultNone
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    #--------
    #let parentSym = args.parentSymSeq[][^1]
    #--------
    let identResult = (
      myAst.myVarEtcDeclMost.ident.myDoIt(none(Symbol))
    )
    let ident = identResult.myIdent
    var sym = Symbol(
      inputFname: self[].inputFname,
      name: ident,
      #initValAst: none(AstNode),
    )
    sym.typeInfo = myAst.myVarEtcDeclMost.type.myDoIt(some(sym)).myType
    result = SymTypeResult(
      scopeAst: identResult.scopeAst,
      kind: stResultSym,
      mySym: sym,
    )
    #case parentSym[].kind:
    #of symVar:
    #  discard
    #of symConst:
    #  discard
    #of symFuncDecl:
    #  discard
    #of symStructDecl:
    #  discard
    #else:
    #  parentEek()
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()
#--------

proc doPassSymTypeMain(
  args: var SymTypeArgs
): SymTypeResult =
  result = SymTypeResult(kind: stResultNone)
  case myAst.kind:
  of astSrcFile:
    # `module`: come back to this for when multiple Scone source files are
    # possible to be used in a single program!
    result = args.doAstSrcFile()
  of astIdent:
    result = args.doAstIdent()
  of astU64Lit:
    result = args.doAstU64Lit()
  of astStrLit:
    result = args.doAstStrLit()
  of astTrue:
    result =args.doAstTrue()
  of astFalse:
    result = args.doAstFalse()
  #of astPtr:
  #  result = args.doAstPtr()
  of astDeref:
    result = args.doAstDeref()
  of astDot:
    result = args.doAstDot()
  of astVar:
    result = args.doAstVar()
  of astConst:
    result = args.doAstConst()
  of astDef:
    result = args.doAstDef()
  of astModule:
    result = args.doAstModule()
  of astStruct:
    result = args.doAstStruct()
  of astEnum:
    result = args.doAstEnum()
  of astExtern:
    result = args.doAstExtern()
  of astCextern:
    result = args.doAstCextern()
  of astImport:
    result = args.doAstImport()
  of astCImport:
    result = args.doAstCImport()
  of astScope:
    result = args.doAstScope()
  of astIf:
    result = args.doAstIf()
  of astElif:
    result = args.doAstElif()
  of astElse:
    result = args.doAstElse()
  of astSwitch:
    result = args.doAstSwitch()
  of astCase:
    result = args.doAstCase()
  of astDefault:
    result = args.doAstDefault()
  of astFor:
    result = args.doAstFor()
  of astWhile:
    result = args.doAstWhile()
  of astContinue:
    result = args.doAstContinue()
  of astBreak:
    result = args.doAstBreak()
  of astReturn:
    result = args.doAstReturn()
  of astArray:
    result = args.doAstArray()
  of astUnop:
    result = args.doAstUnop()
  of astBinop:
    result = args.doAstBinop()
  of astAssignEtc:
    result = args.doAstAssignEtc()
  of astBasicType:
    result = args.doAstBasicType()
  of astNamedType:
    result = args.doAstNamedType()
  of astType:
    result = args.doAstType()
  of astFuncCall:
    result = args.doAstFuncCall()
  of astStmtExprLhs:
    result = args.doAstStmtExprLhs()
  of astFuncNamedArgImpl:
    result = args.doAstFuncNamedArgImpl()
  of astGenericNamedArgImpl:
    result = args.doAstGenericNamedArgImpl()
  of astGenericList:
    result = args.doAstGenericList()
  of astVarEtcDeclMost:
    result = args.doAstVarEtcDeclMost()

  #if result.isScope:
  #  args.self[].gotoParentSymTbl()

proc doPassSymType*(
  self: var Scone,
) =
  var parentAstSeq: seq[AstNode]
  var parentSymSeq: seq[Symbol]
  var moduleIdent: string
  var myArgs = SymTypeArgs(
    self: addr self,
    changed: false,
    subPass: SconeSubPassSymType(0u),
    ast: self.astRoot,
    parentAstSeq: addr parentAstSeq,
    parentSymSeq: addr parentSymSeq,
    moduleIdent: addr moduleIdent
  )
  let subPass = addr myArgs.subPass
  while subPass[] < limSpSymType:
    self.nextSymTblPass()
    discard myArgs.doPassSymTypeMain()
    myArgs.parentAstSeq[].setLen(0)
    myArgs.parentSymSeq[].setLen(0)
    #myArgs.parentResultSeq[].setLen(0)

    var tempSubPass = uint(subPass[])
    tempSubPass += 1
    subPass[] = SconeSubPassSymType(tempSubPass)
    if subPass[] == limSpSymType:
      if myArgs.changed:
        #subPass[] = SconeSubPassSymType(2u)
        subPass[] = spstSubstGenerics
