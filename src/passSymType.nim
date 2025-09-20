import std/tables
import std/sets
import std/options

import scone
import dataStructuresMisc
import ast
import typeInfo
import symTbl
import reduceEtc

type
  SymTypeArgs = object
    self*: ptr Scone
    changed*: bool
    topLevelChanged*: bool
    #subPass*: SconeSubPassSymType
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
    ast*: AstNode
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

template subPass(
  args: untyped
): untyped =
  args.self[].symTypeSubPass

template eek(): untyped =
  doAssert(
    false,
    "eek! " & $args.subPass & " " & $args.ast
  )
template parentEek(): untyped =
  doAssert(
    false,
    sconcat(@[
      "eek! ",
      $parentSym[].kind,
      " ",
      $parentSym[].typeInfo[],
      ";  ",
      $args.parentAstSeq[][^1]
    ])
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
    #subPass: args.subPass,
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
#  #  ast: myAst,
#  #)
#  #self[].addChildSymTbl(hiddenSymTbl)
#  #hiddenSymTbl
#  #result.isScope =
#  discard args.self[].mkSymbolTableMain(ast=myAst)
#  defer: args.self[].gotoParentSymTbl()

template mkScopePre(
  argSym: Option[Symbol],
  argAst: AstNode,
): SymbolTable =
  #let curr = args.self[].mySymTblInfo.curr
  args.self[].addSym(
    sym=argSym,
    ast=argAst,
  )
  #let curr = args.self[].mySymTblInfo.curr
  let curr = args.self[].mySymTblInfo.curr#.parent
  curr

template mkScopePost(
  argSym: Option[Symbol],
  argAst: AstNode,
): untyped =
  #defer:
  let curr = args.self[].mySymTblInfo.curr#.parent
  if argSym.isSome:
    if not args.self[].inFindAllDecls:
      args.self[].checkDuplSym()
    #if isGenericList:
    #  #case argSym.get().kind:
    #  #of symStructDecl:
    #  #  argSym.get().typeInfo
    #  #of symFuncDecl:
    #  #  discard
    #  #else:
    #  #  discard
    #if args.subPass == spstFindTopLevelDecls:
    case args.subPass:
    of spstFindTopLevelDecls:
      #if argSym.isSome:#curr.sym.isSome:
      #echo sconcat(@[
      #  "mkScopePost(): parent.sym.isSome: ",
      #  $curr.parent.sym.isSome
      #])
      if curr.parent.sym.isSome:
        #echo sconcat(@[
        #  "mkScopePost: parent.sym.get(): ",
        #  $curr.parent.sym.get()[]
        #])
        #echo sconcat(@[
        #  "mkScopePost: parent.sym.get().typeInfo: ",
        #  $curr.parent.sym.get().typeInfo[]
        #])
        #if child.sym.get().kind == symGeneric:
        #case argSym.get().kind:
        if curr.sym.isSome:
          #echo sconcat(@[
          #  "mkScopePost: sym.get(): ",
          #  $curr.sym.get()[]
          #])
          #echo sconcat(@[
          #  "mkScopePost: sym.get().typeInfo: ",
          #  $curr.sym.get().typeInfo[]
          #])
          case curr.sym.get().kind:
          #of symGenericImpl:
          #  #let tinfo = argSym.get().typeInfo
          #  discard
          of symGenericDecl:
            let tinfo = curr.parent.sym.get().typeInfo
            #let tinfo = argSym.get().typeInfo

            case tinfo.kind:
            of tiStruct:
              tinfo.myStruct.genericIdxSeq.add(
                curr.parent.childSeq.len() - 1
              )
            of tiFunc:
              tinfo.myFunc.genericIdxSeq.add(
                curr.parent.childSeq.len() - 1
              )
            else:
              #eek()
              discard
          else:
            discard
        else:
          discard
        #echo ""
      #else:
      #  echo sconcat(@[
      #    "not curr.parent.sym.isSome: ",
      #    $curr.sym.isSome
      #  ])
      #  if curr.sym.isSome:
      #    echo $curr.sym.get()[]
    else:
      discard
  args.self[].gotoParentSymTbl()
  #curr

#template handleGenerics(
#  argSym: Symbol
#): untyped =
  

proc doAstSrcFile(
  args: var SymTypeArgs
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #mkSymbolTable()
  let self = args.self
  self[].inFindAllDecls = (
    args.subPass == spstFindTopLevelDecls
  )

  #discard mkScopePre(none(Symbol), myAst)
  #defer:
  #  mkScopePost(none(Symbol), myAst)
  #var mySymTbl = SymbolTable(
  #  ast: 
  #)
  #self[].nextSymTblPass()
  discard myAst.mySrcFile.module.myDoIt(none(Symbol))
  #--------
  args.topLevelChanged = true
  while args.topLevelChanged:
    args.topLevelChanged = false
    for structDecl in myAst.mySrcFile.structDeclSeq:
      discard structDecl.myDoIt(none(Symbol))
  #--------
  args.topLevelChanged = true
  while args.topLevelChanged:
    args.topLevelChanged = false
    for funcDecl in myAst.mySrcFile.funcDeclSeq:
      discard funcDecl.myDoIt(none(Symbol))
  #--------
  #case args.subPass:
  #of spstFindTopLevelDecls:
  #  #--------
  #  args.topLevelChanged = true
  #  while args.topLevelChanged:
  #    args.topLevelChanged = false
  #    for structDecl in myAst.mySrcFile.structDeclSeq:
  #      discard structDecl.myDoIt(none(Symbol))
  #  #--------
  #  args.topLevelChanged = true
  #  while args.topLevelChanged:
  #    args.topLevelChanged = false
  #    for funcDecl in myAst.mySrcFile.funcDeclSeq:
  #      discard funcDecl.myDoIt(none(Symbol))
  #  #--------
  #else:
  #  #--------
  #  if args.subPass == spstSubstGenerics:
  #    args.changed = false
  #  #--------
  #  for structDecl in myAst.mySrcFile.structDeclSeq:
  #    discard structDecl.myDoIt(none(Symbol))
  #  #--------
  #  for funcDecl in myAst.mySrcFile.funcDeclSeq:
  #    discard funcDecl.myDoIt(none(Symbol))
  #  #--------

proc doAstIdent(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultIdent,
    myIdent: myAst.myIdent.strVal,
  )
proc doAstU64Lit(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultU64,
    myU64: myAst.myU64Lit.u64Val,
  )
proc doAstStrLit(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultString,
    myString: myAst.myStrLit.strLitVal,
  )
proc doAstOpenarrLit(
  args: var SymTypeArgs,
): SymTypeResult =
  #result = SymTypeResult(
  #  ast: myAst,
  #  kind: stResultString,
  #  myString: myAst.myStrLit.strLitVal,
  #)
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )

proc doAstTrue(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultBool,
    myBool: true,
  )
proc doAstFalse(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultBool,
    myBool: false,
  )

proc doAstDeref(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
proc doAstDot(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
proc doAstVar(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
proc doAstConst(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
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
    let curr = mkScopePre(some(sym), myAst)
    defer:
      mkScopePost(some(sym), myAst)
    block:
      if myAst.myDef.genericDecl.isSome:
        discard myAst.myDef.genericDecl.get().myDoIt(some(sym))
    block:
      for idx in 0 ..< myAst.myDef.argDeclSeq.len():
        var mySym = myAst.myDef.argDeclSeq[idx].myDoIt(some(sym))
        var tempScope = mkScopePre(some(mySym.mySym), mySym.ast)
        defer:
          mkScopePost(some(mySym.mySym), mySym.ast)
        sym.typeInfo.myFunc.argIdxSeq.add curr.childSeq.len() - 1
    block:
      var resultSym = Symbol(
        inputFname: self[].inputFname,
        name: "<result>",
        kind: symVar,
      )
      resultSym.typeInfo = myAst.myDef.returnType.myDoIt(some(sym)).myType
      discard mkScopePre(some(resultSym), myAst.myDef.returnType)
      defer:
        mkScopePost(some(resultSym), myAst.myDef.returnType)
      sym.typeInfo.myFunc.resultIdx = curr.childSeq.len() - 1
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
    ast: myAst,
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
    ast: myAst,
    kind: stResultNone
  )
  let self = args.self
  let info = addr self[].mySymTblInfo

  case args.subPass:
  of spstFindTopLevelDecls:
    var sym = Symbol(
      inputFname: self[].inputFname,
      name: myAst.myStruct.ident.myIdent.strVal,
      kind: symStructDecl,
    )
    sym.typeInfo = TypeInfo(
      main: TypeInfoMain(
        inputFname: self[].inputFname,
        name: some(myAst.myStruct.ident.myIdent.strVal),
        funcVar: false,
        ptrDim: 0,
        arrDim: 0,
      ),
      kind: tiStruct,
    )
    let curr = mkScopePre(some(sym), myAst)
    defer:
      mkScopePost(some(sym), myAst)
    block:
      if myAst.myStruct.genericDecl.isSome:
        discard myAst.myStruct.genericDecl.get().myDoIt(some(sym))
      #for idx in 0 ..< curr.childSeq.len():
      #  discard
    block:
      for idx in 0 ..< myAst.myStruct.fieldSeq.len():
        var mySym = myAst.myStruct.fieldSeq[idx].myDoIt(some(sym))
        discard mkScopePre(some(mySym.mySym), mySym.ast)
        defer:
          mkScopePost(some(mySym.mySym), mySym.ast)
        sym.typeInfo.myStruct.fieldIdxSeq.add curr.childSeq.len() - 1
  else:
    discard
    # TODO (maybe): support struct definitions besides at of the top level
    # of a `module`?
    #for name, idxSeq in info[].prev.nameTbl:
    #  for idx in idxSeq:
    #    let child = info[].prev.childSeq[idx]
    #    if child.sym.isSome:
    #      let sym = child.sym.get()
    #      if sym.kind != symStructDecl:
    #        continue
    #      if name notin info[].curr.nameTbl:
    #        info[].curr.nameTbl[name] = @[info[].curr.childSeq.len()]
    #      else:
    #        info[].curr.nameTbl[name].add info[].curr.childSeq.len()
    #      info[].curr.childSeq.add info[].prev.childSeq[idx]
  #let self = args.self
  #if args.self.symS2d
proc doAstEnum(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstVariant(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
proc doAstExtern(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstCextern(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstImport(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstCImport(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstScope(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstIf(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstElif(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstElse(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstSwitch(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstCase(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstDefault(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstFor(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstWhile(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstContinue(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstBreak(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstReturn(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstArray(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
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
proc doAstOpenarray(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone,
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    #result = myAst.myOpenarray.elemType.myDoIt(none(Symbol))
    discard
  of spstSubstGenerics:
    discard
  of spstHandleFuncOverloading:
    discard
  of spstTypeCheck:
    discard
  else:
    eek()
proc doAstBuiltinTypeCast(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone,
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    #result = myAst.myOpenarray.elemType.myDoIt(none(Symbol))
    discard
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
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstBinop(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstAssignEtc(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstBasicType(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  let self = args.self
  case args.subPass:
  of spstFindTopLevelDecls:
    result = SymTypeResult(
      ast: myAst,
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
        #ast: myAst
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
    ast: myAst,
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
      ast: myAst,
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
        #ast: myAst,
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
    ast: myAst,
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
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstStmtExprLhs(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstFuncNamedArgImpl(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstGenericNamedArgImpl(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
    kind: stResultNone
  )
  #let self = args.self
proc doAstGenericList(
  args: var SymTypeArgs,
): SymTypeResult =
  result = SymTypeResult(
    ast: myAst,
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
      of symVar, symConst, symFuncCall, symFuncCallArg:
        sym.kind = symGenericImpl
      of symStructDecl, symFuncDecl:
        # TODO: support other kinds of top-level symbol declarations,
        # probably mostly `variant`s
        sym.kind = symGenericDecl #symStructGeneric
        #let parentTinfo = parentSym[].typeInfo
        #case parentTinfo.kind:
        #of 
        #case parentSym[].kind:
        #of symVar:
        #  discard
        #of symConst:
        #  discard
        #of symStructDecl:
        #  discard
      #of symFuncDecl:
      #  sym.kind = symGeneric #symFuncGeneric
      else:
        parentEek()

      #case parentSym[].kind:
      #of symVar:
      #  discard
      #of symConst:
      #  discard
      #of symStructDecl:
      #  discard
      #of symFuncDecl:
      #  discard
      #else:
      #  parentEek()

      discard mkScopePre(
        some(sym),
        identResult.ast
        #myAst #
      )
      defer: mkScopePost(
        some(sym),
        identResult.ast
        #myAst #
      )
      #self[].addSym(some(sym))
    of spstSubstGenerics:
      #case parentSym[].kind:
      #else:
      #  discard
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
    ast: myAst,
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
      ast: identResult.ast,
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
  if myAst == nil:
    return
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
  of astOpenarrLit:
    result = args.doAstOpenarrLit()
  of astTrue:
    result = args.doAstTrue()
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
  of astVariant:
    result = args.doAstVariant()
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
  of astOpenarray:
    result = args.doAstOpenarray()
  of astBuiltinTypeCast:
    result = args.doAstBuiltinTypeCast()
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
    #subPass: SconeSubPassSymType(0u),
    ast: self.astRoot,
    parentAstSeq: addr parentAstSeq,
    parentSymSeq: addr parentSymSeq,
    moduleIdent: addr moduleIdent
  )
  #myArgs.subPass = SconeSubPassSymType(0u)

  let subPass = addr myArgs.subPass
  subPass[] = SconeSubPassSymType(0u)
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
