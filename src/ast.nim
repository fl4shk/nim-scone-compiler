import std/options
import std/macros
import std/strutils

import dataStructuresMisc


#type
#  AstLitValKind* = enum
#    astLvKindI64,
#    astLvKindU64,
#    #astLvKindF32,
#    #astLvKindF64,
#    astLvKindStr,
#
#  AstLitVal* = object
#    case kind: AstLitValKind
#    of astLvKindI64:
#      i64Val: int64
#    of astLvKindU64:
#      u64Val: uint64
#    #of astLvKindF32:
#    #  f32Val: string
#    #of astLvKindF64:
#    #  f64Val: string
#    of astLvKindStr:
#      strVal: string

#type
#  AstIdent* = object
#    



#type
#  AstSrcFile* = object
#    module*: AstNode
#    funcDeclSeq*: seq[AstNode]
#    structDeclSeq*: seq[AstNode]
#
#  AstIdent* = object
#    strVal*: string
#  AstU64Lit* = object
#    u64Val*: uint64
#  AstStrLit* = object
#    strLitVal*: string
##--------
#  AstTrue* = object
#  AstFalse* = object
##--------
#  #AstLBracket* = object
#  #AstRBracket* = object
##--------
#  AstPtr* = object
#  AstAddr* = object
#  AstDeref* = object
#  AstDot* = object
##--------
#  AstVar* = object
#    ident*: AstNode                # `AstIdent`
#    myType*: AstNode               # `AstType`
#    optExpr*: Option[AstNode]      # optional expression
#  AstConst* = object
#    ident*: AstNode                # `AstIdent`
#    myType*: AstNode               # `AstType`
#    expr*: AstNode                 # expression
#  AstDef* = object
#    ident*: AstNode                # `AstIdent`
#    genericDeclSeq*: seq[AstNode]  # seq of `AstIdent`
#    argDeclSeq*: seq[AstNode]      # seq of `AstVar`
#    stmtSeq*: seq[AstNode]
#
##("Macro", some("macro")),
#    # Maybe save `macro` for the bootstrapped compiler?
#    # I'm not sure I outright need macros for this version of the
#    # compiler
#  AstModule* = object
#    ident*: AstNode                # `AstIdent`
#
#  AstStruct* = object
#    genericDeclSeq*: seq[AstNode]  # seq of `AstIdent`
#    fieldSeq*: seq[AstNode]        # `seq` of `AstVar`
#
#  AstEnum* = object
#    # TODO: come back to this later
#  AstExtern* = object
#    # TODO: come back to this later
#  AstCextern* = object
#    # TODO: come back to this later
#  AstImport* = object
#    # TODO: come back to this later
#  AstCImport* = object
#    # TODO: come back to this later
##--------
#  AstScope* = object
#    # TODO: come back to this later
#  AstIf* = object
#    expr*: AstNode                 # (condition) expression
#    stmtSeq*: seq[AstNode]         # the list of statements within the scope
#    optChild*: Option[AstNode]     # optional `AstElif` or `AstElse`
#  AstElif* = object
#    expr*: AstNode                 # (condition) expression
#    stmtSeq*: seq[AstNode]         # the list of statements within the scope
#    optChild*: Option[AstNode]     # optional `AstElif` or `AstElse`
#  AstElse* = object
#    stmtSeq*: seq[AstNode]         # the list of statements within the scope
#  AstSwitch* = object
#    expr*: AstNode                 # the condition
#    childSeq*: seq[AstNode]        # the list of `case` or `default`
#  AstCase* = object
#    expr*: AstNode                 # the condition
#    stmtSeq*: seq[AstNode]         # the list of statements within the scope
#  AstDefault* = object
#    stmtSeq*: seq[AstNode]         # the list of statements within the scope
#  AstFor* = object
#    ident*: AstNode                # name of the indexing variable
#    exprPre*: AstNode              # 
#    exprPost*: AstNode             #
#    isUntil*: bool
#    stmtSeq*: seq[AstNode]
#  AstWhile* = object
#    expr*: AstNode                 # (conditional) expression
#    stmtSeq*: seq[AstNode]         # the list of statements within the scope
#  AstContinue* = object
#  AstBreak* = object
#  #AstResult* = object
#  AstReturn* = object
#    optExpr*: Option[AstNode]
##--------
#  AstArray* = object
#    dim*: AstNode
#    elemType*: AstNode
#
#  AstVoid* = object
#  AstBool* = object
#  AstU8* = object
#  AstI8* = object
#  AstU16* = object
#  AstI16* = object
#  AstU32* = object
#  AstI32* = object
#  AstU64* = object
#  AstI64* = object
#  AstF32* = object
#  AstF64* = object
#  AstChar* = object
#  AstString* = object
##--------
#
#  #AstUnopKind* = enum
#  #  unopPlus,
#  #  unopMinus,
#  #  unopLogicNot,
#  #  unopBitInvert,
#
#  AstUnop* = object
#    #tok*: TokKind
#    kind*: AstUnopKind
#    child*: AstNode
#
#
#  #AstBinopKind* = enum
#  #  binopCmpEq,
#  #  binopCmpNe,
#  #  binopCmpLt,
#  #  binopCmpGt,
#  #  binopCmpLe,
#  #  binopCmpGe,
#  #  #--------
#  #  binopPlus,
#  #  binopMinus,
#  #  binopMul,
#  #  binopDiv,
#  #  binopMod,
#  #  binopBitAnd,
#  #  binopBitOr,
#  #  binopBitXor,
#  #  binopLogicAnd,
#  #  binopLogicOr,
#  #  binopBitShl,
#  #  binopBitShr,
#
#  AstBinop* = object
#    #tok*: TokKind
#    kind*: AstBinopKind
#    left*: AstNode
#    right*: AstNode
#
##--------
#  #AstAssignEtcKind* = enum
#  #  assignEtcRegular,
#  #  assignEtcPlus,
#  #  assignEtcMinus,
#  #  assignEtcMul,
#  #  assignEtcDiv,
#  #  assignEtcMod,
#  #  assignEtcBitAnd,
#  #  assignEtcBitOr,
#  #  assignEtcBitXor,
#  #  assignEtcBitShl,
#  #  assignEtcBitShr,
#  AstAssignEtc* = object
#    kind*: AstAssignEtcKind
#    left*: AstNode
#    right*: AstNode
#
#  AstNamedType* = object
#    ident*: AstNode
#    genericImplSeq*: seq[AstNode]
#
#  AstType* = object
#    #ident*: AstNode
#    #genericImplSeq*: seq[AstNode]
#    childSeq*: seq[AstNode]
#
#  AstFuncCall* = object
#    ident*: AstNode
#    genericImplSeq*: seq[AstNode]
#    argImplSeq*: seq[AstNode]
#
#  AstNode* = ref AstNodeObj
#  AstNodeObj* = object
#    #tok*: TokKind
#    #kind*: AstKind
#    #lineNum*: uint64
#    lexMain*: LexMain
#    #u64Val*: uint64
#    #strVal*: string
#    #litVal*: Option[AstLitVal]
#    #symIdxSeq*: seq[uint64]
#    #chIdxSeq*: seq[uint64]    # indices into `Scone.ast` children
#    parent*: AstNode
#
#    case kind*: AstKind:
#    of astSrcFile: srcFileVal*: AstSrcFile
#    of astIdent: identVal*: AstIdent    # identifiers
#    of astU64Lit: u64LitVal*: AstU64Lit # 0-9, hex numbers, binary numbers, etc.
#    of astStrLit: strLitVal*: AstStrLit # string literals
#    #--------
#    of astTrue: trueVal*: AstTrue
#    of astFalse: falseVal*: AstFalse
#    #--------
#    #of astLBracket: lBracketVal*: AstLBracket
#    #of astRBracket: rBracketVal*: AstRBracket
#    #--------
#    of astPtr: ptrVal*: AstPtr
#    of astAddr: addrVal*: AstAddr
#    of astDeref: derefVal*: AstDeref # pointer dereference
#    of astDot: dotVal*: AstDot
#    #--------
#    of astVar: varVal*: AstVar
#    of astConst: constVal*: AstConst
#    of astDef: defVal*: AstDef
#    #("Macro", some("macro")),
#        # Maybe save `macro` for the bootstrapped compiler?
#        # I'm not sure I outright need macros for this version of the
#        # compiler
#    of astModule: moduleVal*: AstModule
#    of astStruct: structVal*: AstStruct
#    of astEnum: enumVal*: AstEnum
#    of astExtern: externVal*: AstExtern
#    of astCextern: cexternVal*: AstCextern
#    of astImport: importVal*: AstImport
#    of astCImport: cImportVal*: AstCImport
#    #--------
#    of astScope: scopeVal*: AstScope
#    of astIf: ifVal*: AstIf
#    of astElif: elifVal*: AstElif
#    of astElse: elseVal*: AstElse
#    of astSwitch: switchVal*: AstSwitch
#    of astCase: caseVal*: AstCase
#    of astDefault: defaultVal*: AstDefault
#    of astFor: forVal*: AstFor
#    of astWhile: whileVal*: AstWhile
#    of astContinue: continueVal*: AstContinue
#    of astBreak: breakVal*: AstBreak
#    #of astResult: resultVal*: AstResult
#    of astReturn: returnVal*: AstReturn
#    #--------
#    of astArray: arrayVal*: AstArray
#    of astVoid: voidVal*: AstVoid
#    of astBool: boolVal*: AstBool
#    of astU8: u8Val*: AstU8
#    of astI8: i8Val*: AstI8
#    of astU16: u16Val*: AstU16
#    of astI16: i16Val*: AstI16
#    of astU32: u32Val*: AstU32
#    of astI32: i32Val*: AstI32
#    of astU64: u64Val*: AstU64
#    of astI64: i64Val*: AstI64
#    of astF32: f32Val*: AstF32
#    of astF64: f64Val*: AstF64
#    of astChar: charVal*: AstChar
#    of astString: stringVal*: AstString
#    #--------
#    of astBinop: binopVal*: AstBinop
#    of astUnop: unopVal*: AstUnop
#    of astAssignEtc: assignEtcVal*: AstAssignEtc
#    of astNamedType: namedTypeVal*: AstNamedType
#    of astType: typeVal*: AstType
#    of astFuncCall: funcCallVal*: AstFuncCall

proc tokToUnop*(
  tok: TokKind,
): Option[AstUnopKind] =
  result = none(AstUnopKind)

  case tok:
  of tokPlus:
    return some(unopPlus)
  of tokMinus:
    return some(unopMinus)
  of tokLogicNot:
    return some(unopLogicNot)
  of tokBitInvert:
    return some(unopBitInvert)
  else:
    return none(AstUnopKind)

proc tokToBinop*(
  tok: TokKind,
): Option[AstBinopKind] =
  result = none(AstBinopKind)
  case tok:
  of tokCmpEq:
    return some(binopCmpEq)
  of tokCmpNe:
    return some(binopCmpNe)
  of tokCmpLt:
    return some(binopCmpLt)
  of tokCmpGt:
    return some(binopCmpGt)
  of tokCmpLe:
    return some(binopCmpLe)
  of tokCmpGe:
    return some(binopCmpGe)
  #--------
  of tokPlus:
    return some(binopPlus)
  of tokMinus:
    return some(binopMinus)
  of tokMul:
    return some(binopMul)
  of tokDiv:
    return some(binopDiv)
  of tokMod:
    return some(binopMod)
  of tokBitAnd:
    return some(binopBitAnd)
  of tokBitOr:
    return some(binopBitOr)
  of tokBitXor:
    return some(binopBitXor)
  of tokLogicAnd:
    return some(binopLogicAnd)
  of tokLogicOr:
    return some(binopLogicOr)
  of tokBitShl:
    return some(binopBitShl)
  of tokBitShr:
    return some(binopBitShr)
  else:
    return none(AstBinopKind)
proc tokToAssignEtc*(
  tok: TokKind,
): Option[AstAssignEtcKind] =
  result = none(AstAssignEtcKind)

  case tok:
  of tokAssign:
    return some(assignEtcRegular)
  of tokAssignPlus:
    return some(assignEtcPlus)
  of tokAssignMinus:
    return some(assignEtcMinus)
  of tokAssignMul:
    return some(assignEtcMul)
  of tokAssignDiv:
    return some(assignEtcDiv)
  of tokAssignMod:
    return some(assignEtcMod)
  of tokAssignBitAnd:
    return some(assignEtcBitAnd)
  of tokAssignBitOr:
    return some(assignEtcBitOr)
  of tokAssignBitXor:
    return some(assignEtcBitXor)
  of tokAssignBitShl:
    return some(assignEtcBitShl)
  of tokAssignBitShr:
    return some(assignEtcBitShr)
  else:
    return none(AstAssignEtcKind)

#  TypeSection
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "AstSrcFile"
#      Empty
#      ObjectTy
#        Empty
#        Empty
#        RecList
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "module"
#            Ident "AstNode"
#            Empty
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "funcDeclSeq"
#            BracketExpr
#              Ident "seq"
#              Ident "AstNode"
#            Empty
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "structDeclSeq"
#            BracketExpr
#              Ident "seq"
#              Ident "AstNode"
#            Empty
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "AstIdent"
#      Empty
#      ObjectTy
#        Empty
#        Empty
#        RecList
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "strVal"
#            Ident "string"
#            Empty



#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "AstNode"
#      Empty
#      RefTy
#        Ident "AstNodeObj"
#    TypeDef
#      Postfix
#        Ident "*"
#        Ident "AstNodeObj"
#      Empty
#      ObjectTy
#        Empty
#        Empty
#        RecList
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "lexMain"
#            Ident "LexMain"
#            Empty
#          IdentDefs
#            Postfix
#              Ident "*"
#              Ident "parent"
#            Ident "AstNode"
#            Empty
#          RecCase
#            IdentDefs
#              Postfix
#                Ident "*"
#                Ident "kind"
#              Ident "AstKind"
#              Empty
#            OfBranch
#              Ident "astSrcFile"
#              IdentDefs
#                Postfix
#                  Ident "*"
#                  Ident "srcFileVal"
#                Ident "AstSrcFile"
#                Empty
#            OfBranch
#              Ident "astIdent"
#              IdentDefs
#                Postfix
#                  Ident "*"
#                  Ident "identVal"
#                Ident "AstIdent"
#                Empty
#            OfBranch
proc mkPubIdent(
  name: string
): NimNode =
  result = add(
    add(newNimNode(nnkPostfix),
      ident("*"), ident(name)
    )
  )

macro mkAstHier(): untyped =
  #result = newTypeSection(
  #)
  #result = quote do:
  #  discard
  result = newNimNode(nnkTypeSection)
  #var typeDefSeq: seq[NimNode]
  for idx in 0 ..< helperTokKindSeq.len():
    let h = helperTokKindSeq[idx]
    if not h[2]:
      continue

    var typeDef = newNimNode(nnkTypeDef)

    typeDef.add mkPubIdent("Ast" & h[0])
    typeDef.add newNimNode(nnkEmpty)
    #var objTyArgs: seq[NimNode]
    #objTyArgs

    var objTy = add(
      newNimNode(nnkObjectTy),
      [
        newNimNode(nnkEmpty),
        newNimNode(nnkEmpty),
        #newNimNode(nnkRecList),
      ]
    )
    #objTy.dumpTree()

    #for arg in objTyArgs:
    #  objTy.add arg
    template recList(): untyped = objTy[2]
    var didAddRecList: bool = false
    for jdx in 0 ..< h[3].len():
      if jdx == 0:
        didAddRecList = true
        objTy.add(newNimNode(nnkRecList))
      let field = h[3][jdx]
      #echo "jdx:" & $jdx & " " & $h
      #echo "field:" & $field

      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add mkPubIdent(field[0])
      #echo identDefs.repr()

      case field[1]:
      of astValAstNode:
        identDefs.add ident("AstNode")
      of astValSeqAstNode:
        identDefs.add(
          add(
            newNimNode(nnkBracketExpr),
            ident("seq"),
            ident("AstNode"),
          )
        )
      of astValOptAstNode:
        identDefs.add(
          add(newNimNode(nnkBracketExpr),
            ident("Option"),
            ident("AstNode"),
          )
        )
      of astValString:
        identDefs.add ident("string")
      of astValU64:
        identDefs.add ident("uint64")
      of astValBool:
        identDefs.add ident("bool")
      of astValUnopKind:
        identDefs.add ident("AstUnopKind")
      of astValBinopKind:
        identDefs.add ident("AstBinopKind")
      of astValAssignEtcKind:
        identDefs.add ident("AstAssignEtcKind")

      identDefs.add newNimNode(nnkEmpty)
      recList.add identDefs
    if not didAddRecList:
      objTy.add newNimNode(nnkEmpty)
    typeDef.add objTy

    result.add typeDef

  if true:
    var typeDef = newNimNode(nnkTypeDef)
    typeDef.add mkPubIdent("AstNode")
    typeDef.add newNimNode(nnkEmpty)
    typeDef.add(
      add(newNimNode(nnkRefTy), ident("AstNodeObj"))
    )
    result.add typeDef
    #echo result.repr()

  if true:
    var typeDef = newNimNode(nnkTypeDef)
    typeDef.add mkPubIdent("AstNodeObj")
    typeDef.add newNimNode(nnkEmpty)
    var objTy = newNimNode(nnkObjectTy)
    template recList(): untyped = objTy[2]
    objTy.add(
      newNimNode(nnkEmpty), newNimNode(nnkEmpty), newNimNode(nnkRecList)
    )
    if true:
      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add(
        mkPubIdent("lexMain"),
        ident("LexMain"),
        newNimNode(nnkEmpty),
      )
      recList.add identDefs
    #if true:
    #  var identDefs = newNimNode(nnkIdentDefs)
    #  identDefs.add(
    #    mkPubIdent("parentExpr"),
    #    ident("AstNode"),
    #    newNimNode(nnkEmpty),
    #  )
    #  recList.add identDefs
    if true:
      var recCase = newNimNode(nnkRecCase)
      var identDefs = newNimNode(nnkIdentDefs)
      identDefs.add(
        mkPubIdent("kind"),
        ident("AstKind"),
        newNimNode(nnkEmpty),
      )
      recCase.add identDefs
      for idx in 0 ..< helperTokKindSeq.len():
        let h = helperTokKindSeq[idx]
        if not h[2]:
          continue

        var ofBranch = newNimNode(nnkOfBranch)
        ofBranch.add(
          ident("ast" & h[0])
        )
        var obIdentDefs = newNimNode(nnkIdentDefs)
        obIdentDefs.add(
          mkPubIdent("my" & h[0]),
          ident("Ast" & h[0]),
          newNimNode(nnkEmpty)
        )

        ofBranch.add obIdentDefs
        recCase.add ofBranch
      recList.add recCase

    #objTy.add 

    typeDef.add objTy
    result.add typeDef

    #echo result.repr()

  #for idx in 0 ..< helperTokKindSeq.len():
  #  let h = helperTokKindSeq[idx]
  #  if not h[2]:
  #    continue


mkAstHier()

proc doIndent(
  indent: uint,
): string =
  for idx in 0 ..< indent:
    result.add " "

#macro toStr*(
#  ast: AstNode
#): untyped =
#  #dumpTree(ast)
#  case ast.kind:
#  of 
#  discard

proc toStr*(
  astSeq: seq[AstNode],
  indent: uint,
): string

proc toStr*(
  ast: AstNode,
  indent: uint,
): string

#CaseStmt
#  DotExpr
#    Ident "ast"
#    Ident "kind"
#  OfBranch
#    Ident "astSrcFile"
#    StmtList
#      Command
#        DotExpr
#          Ident "result"
#          Ident "add"
#        StrLit "(AstSrcFile\n"
#      Call
#        DotExpr
#          Ident "result"
#          Ident "add"
#        Infix
#          Ident "&"
#          Infix
#            Ident "&"
#            Infix
#              Ident "&"
#              Ident "i"
#              StrLit "(module "
#            Call
#              DotExpr
#                DotExpr
#                  DotExpr
#                    Ident "ast"
#                    Ident "mySrcFile"
#                  Ident "module"
#                Ident "toStr"
#              Ident "x"
#          StrLit ")\n"


#            Call
#              DotExpr
#                Ident "result"
#                Ident "add"
#              Infix
#                Ident "&"
#                Infix
#                  Ident "&"
#                  Infix
#                    Ident "&"
#                    Ident "i"
#                    StrLit "(structDeclSeq "
#                  Call
#                    DotExpr
#                      DotExpr
#                        DotExpr
#                          Ident "ast"
#                          Ident "mySrcFile"
#                        Ident "structDeclSeq"
#                      Ident "toStr"
#                    Ident "x"
#                StrLit ")\n"

proc toStr*(
  ast: AstNode,
  indent: uint,
): string =
  if ast == nil:
    return "nil"
  var x = indent + 2
  #result.add doIndent(indent=indent)
  template i(): untyped =
    doIndent(indent=x)
  #let iFinish = i & ")\n"
  var iFinish = doIndent(indent=indent)

  macro doCaseStmt(): untyped =
    result = newNimNode(nnkCaseStmt)
    result.add(
      add(newNimNode(nnkDotExpr), ident("ast"), ident("kind"))
    )
    #echo result.repr()
    for idx in 0 ..< helperTokKindSeq.len():
      let h = helperTokKindSeq[idx]
      if not h[2]:
        continue

      var ofBranch = newNimNode(nnkOfBranch)
      ofBranch.add ident("ast" & h[0])
      var stmtList = newNimNode(nnkStmtList)

      var myCmd0 = newNimNode(nnkCommand)
      let tempAstName = "(Ast" & h[0]
      let tempNimNode = quote do:
        #i & 
        `tempAstName`
        
      myCmd0.add(
        add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
        #newLit(
        #  "(Ast" & h[0] #& "\n" #& i
        #),
        tempNimNode
      )
      stmtList.add myCmd0
      #stmtList.add quote do:
      #  result.add i

      var tempNl: NimNode
      var tempI: NimNode
        

      for jdx in 0 ..< h[3].len():
        let field = h[3][jdx]
        if jdx == 0 and h[3].len > 1:
          stmtList.add quote do:
            result.add "\n" #`tempNl`
        #else:
        if h[3].len <= 1:
          stmtList.add quote do:
            x = indent
        if h[3].len() > 1:
          #tempNl = "\n"
          tempNl = quote do:
            "\n" #& i
          tempI = quote do:
            i
        else:
          tempNl = quote do:
            #i
            ""
          tempI = quote do:
            " "
        #var myCmdInner = newNimNode(nnkCommand)

        #proc myAddIndent(myCmdInner: var NimNode) =
        #  myCmdInner.add(
        #    add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
        #    ident("i")
        #  )
        #myCmdInner.myAddIndent()

        #var myDotExpr = (
        #  add(newNimNode(nnkDotExpr), ident("result"), ident("add"))
        #)
        var toAdd: NimNode = nil

        #let myResultIdent = ident("result")
        #let myAddIdent = ident("add")
        #let myIIdent = ident("i")
        #let myXIdent = ident("x")
        let myAstIdent = ident("ast")
        let myMbrIdent = ident("my" & h[0])
        let mbrStr = newLit(field[0])
        let myInnerMbrIdent = ident(field[0])
        #let myToStrIdent = ident("toStr")
        let myDualDotExpr = quote do:
          `myAstIdent`.`myMbrIdent`.`myInnerMbrIdent`

        case field[1]:
        of astValAstNode:
          toAdd = quote do:
            result.add(
              (
                #"\n" & i & 
                #`tempNl` & 
                `tempI` & `mbrStr` & " "
              ) & (
                `myDualDotExpr`.toStr(x) #& i & ")\n"
              ) & (
                `tempNl`
              )
            )
        of astValSeqAstNode:
          toAdd = quote do:
            result.add(
              (
                #"\n" & i & 
                #`tempNl` & 
                `tempI` & `mbrStr` & " "
              ) & (
                `myDualDotExpr`.toStr(x) #& "\n"
              ) & (
                `tempNl`
              )
            )
        of astValOptAstNode:
          toAdd = quote do:
            if `myDualDotExpr`.isSome:
              result.add(
                (
                  #"\n" & i & 
                  #`tempNl` & 
                  `tempI` & `mbrStr` & " "
                ) & (
                  `myDualDotExpr`.get.toStr(x)
                ) & (
                  `tempNl`
                )
              )
            else:
              result.add(
                #"\n" & i & 
                (
                  " " & `mbrStr` & " " & "!isSome" #& ")\n"
                ) & (
                  `tempNl`
                )
              )
        of astValString:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              `tempI` & "\"" & `myDualDotExpr` & "\"" & `tempNl`
            )
        of astValU64:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValBool:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValUnopKind:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              #" " & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValBinopKind:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              #" " & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )
        of astValAssignEtcKind:
          toAdd = quote do:
            result.add(
              #`tempNl` & 
              #" " & 
              `tempI` & `mbrStr` & " " & $`myDualDotExpr` & `tempNl`
            )

        #myCmdInner.add myDotExpr
        #myCmdInner.add toAdd

        stmtList.add toAdd
      #if h[3].len() == 0:
      if true:
        var toAdd: NimNode = nil
        if (
          (
            h[3].len() == 0
          ) or (
            (
              h[3].len() == 1
            ) 
            #and (
            #  #not 
            #  (
            #    (
            #      h[3][0][1] == astValAstNode
            #    ) or (
            #      h[3][0][1] == astValSeqAstNode
            #    ) or (
            #      h[3][0][1] == astValOptAstNode
            #    )
            #  )
            #)
          )
        ):
          toAdd = quote do:
            result.add(")")
        else:
          toAdd = quote do:
            result.add(iFinish & ")")
        stmtList.add toAdd

      #var myCmd1 = newNimNode(nnkCommand)
      #myCmd1.add(
      #  add(newNimNode(nnkDotExpr), ident("result"), ident("add")),
      #  #newLit(")\n"),
      #  ident("iFinish"),
      #)
      #stmtList.add myCmd1

      ofBranch.add stmtList
      result.add ofBranch
    #dumpTree result
    #echo "<-- -->"

    #echo result.repr()
  doCaseStmt()

  #case ast.kind:
  #of astSrcFile:
  #  result.add "(AstSrcFile\n"
  #  result.add(
  #    i & "(module " & ast.mySrcFile.module.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(funcDeclSeq " & ast.mySrcFile.funcDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(structDeclSeq " & ast.mySrcFile.structDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add ")\n"
  #of astIdent:
  #  result.add "(AstIdent " & ast.myIdent.strVal & ") "
  #of astU64Lit:
  #  result.add "(AstU64Lit " & $ast.myU64Lit.u64Val & ") "
  #of astStrLit:
  #  result.add "(AstStrLit \"" & ast.myStrLit.strLitVal & "\") "
  #of astTrue:
  #  result.add "(" & helperTokKindSeq[uint(tokTrue)][1].get() & ") "
  #of astFalse:
  #  result.add "(" & helperTokKindSeq[uint(tokFalse)][1].get() & ") "
  #of astPtr:
  #  result.add "(" & helperTokKindSeq[uint(tokPtr)][1].get() & ") "
  #of astAddr:
  #  result.add "(" & helperTokKindSeq[uint(tokAddr)][1].get() & ") "
  #of astDeref:
  #  result.add "(" & helperTokKindSeq[uint(tokDeref)][1].get() & ") "
  #of astDot:
  #  result.add "(" & helperTokKindSeq[uint(tokDot)][1].get() & ") "
  #of astVar:
  #  result.add "(AstVar\n"
  #  result.add(
  #    i & "(ident " & ast.myVar.ident.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(myType " & ast.myVar.myType.toStr(x) & ")\n"
  #  )
  #  if ast.myVar.optExpr.isSome:
  #    result.add(
  #      i & "(optExpr " & ast.myVar.optExpr.get().toStr(x) & ")\n"
  #    )
  #  else:
  #    result.add(
  #      i & "(not optExpr.isSome)\n"
  #    )
  #  result.add ")\n"
  #of astConst:
  #  result.add "(AstConst\n"
  #  result.add(
  #    i & "(ident " & ast.myConst.ident.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(myType " & ast.myConst.myType.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(expr " & ast.myConst.expr.toStr(x) & ")\n"
  #  )
  #  result.add ")\n"
  #of astDef:
  #  result.add "(AstDef\n"
  #  result.add(
  #    i & "(ident " & ast.myDef.ident.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(genericDeclSeq " & ast.myDef.genericDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add(
  #    i & "(argDeclSeq " & ast.myDef.argDeclSeq.toStr(x) & ")\n"
  #  )
  #  result.add ")\n"
  #of astModule:
  #  result.add "(AstModule\n"
  #  result.add ast.myModule.ident.toStr(x)
  #  result.add ")\n"
  #else:
  #  result.add "(Other)"

proc toStr*(
  astSeq: seq[AstNode],
  indent: uint,
): string =
  let x = indent + 2
  let tempIndent = doIndent(indent=indent)
  let tempIndent1 = doIndent(indent=x)
  #result.add tempIndent
  #let i = doIndent(ident=x)

  result.add "[\n"
  for idx in 0 ..< astSeq.len():
    result.add tempIndent1 & astSeq[idx].toStr(x) & "\n"
  #if int(indent - 2) > 0:
  #  result.add doIndent(indent - 2)
  result.add tempIndent
  result.add "]"
