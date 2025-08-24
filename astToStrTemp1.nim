case ast.kind
of astSrcFile:
  result.add "(AstSrcFile"
  result.add(("\n" & i & "module" & " ") & (ast.mySrcFile.module.toStr(x)))
  result.add(("\n" & i & "funcDeclSeq" & " ") &
      (ast.mySrcFile.funcDeclSeq.toStr(x)))
  result.add(("\n" & i & "structDeclSeq" & " ") &
      (ast.mySrcFile.structDeclSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astIdent:
  result.add "(AstIdent"
  result.add(" \"" & ast.myIdent.strVal & "\"")
  result.add(")")
of astU64Lit:
  result.add "(AstU64Lit"
  result.add(" " & $ast.myU64Lit.u64Val)
  result.add(")")
of astStrLit:
  result.add "(AstStrLit"
  result.add(" \"" & ast.myStrLit.strLitVal & "\"")
  result.add(")")
of astTrue:
  result.add "(AstTrue"
  result.add(")")
of astFalse:
  result.add "(AstFalse"
  result.add(")")
of astPtr:
  result.add "(AstPtr"
  result.add(")")
of astAddr:
  result.add "(AstAddr"
  result.add(")")
of astDeref:
  result.add "(AstDeref"
  result.add(")")
of astDot:
  result.add "(AstDot"
  result.add(")")
of astVar:
  result.add "(AstVar"
  result.add(("\n" & i & "ident" & " ") & (ast.myVar.ident.toStr(x)))
  result.add(("\n" & i & "myType" & " ") & (ast.myVar.myType.toStr(x)))
  if ast.myVar.optExpr.isSome:
    result.add(("\n" & i & "optExpr" & " ") & (ast.myVar.optExpr.get.toStr(x)))
  else:
    result.add("\n" & i & "optExpr" & " " & "!isSome")
  result.add("\n" & iFinish & ")")
of astConst:
  result.add "(AstConst"
  result.add(("\n" & i & "ident" & " ") & (ast.myConst.ident.toStr(x)))
  result.add(("\n" & i & "myType" & " ") & (ast.myConst.myType.toStr(x)))
  result.add(("\n" & i & "expr" & " ") & (ast.myConst.expr.toStr(x)))
  result.add("\n" & iFinish & ")")
of astDef:
  result.add "(AstDef"
  result.add(("\n" & i & "ident" & " ") & (ast.myDef.ident.toStr(x)))
  result.add(("\n" & i & "genericDeclSeq" & " ") &
      (ast.myDef.genericDeclSeq.toStr(x)))
  result.add(("\n" & i & "argDeclSeq" & " ") & (ast.myDef.argDeclSeq.toStr(x)))
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myDef.stmtSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astModule:
  result.add "(AstModule"
  result.add(("\n" & i & "ident" & " ") & (ast.myModule.ident.toStr(x)))
  result.add("\n" & iFinish & ")")
of astStruct:
  result.add "(AstStruct"
  result.add(("\n" & i & "genericDeclSeq" & " ") &
      (ast.myStruct.genericDeclSeq.toStr(x)))
  result.add(("\n" & i & "fieldSeq" & " ") & (ast.myStruct.fieldSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astEnum:
  result.add "(AstEnum"
  result.add(")")
of astExtern:
  result.add "(AstExtern"
  result.add(")")
of astCextern:
  result.add "(AstCextern"
  result.add(")")
of astImport:
  result.add "(AstImport"
  result.add(")")
of astCImport:
  result.add "(AstCImport"
  result.add(")")
of astScope:
  result.add "(AstScope"
  result.add(")")
of astIf:
  result.add "(AstIf"
  result.add(("\n" & i & "expr" & " ") & (ast.myIf.expr.toStr(x)))
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myIf.stmtSeq.toStr(x)))
  if ast.myIf.optChild.isSome:
    result.add(("\n" & i & "optChild" & " ") & (ast.myIf.optChild.get.toStr(x)))
  else:
    result.add("\n" & i & "optChild" & " " & "!isSome")
  result.add("\n" & iFinish & ")")
of astElif:
  result.add "(AstElif"
  result.add(("\n" & i & "expr" & " ") & (ast.myElif.expr.toStr(x)))
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myElif.stmtSeq.toStr(x)))
  if ast.myElif.optChild.isSome:
    result.add(("\n" & i & "optChild" & " ") &
        (ast.myElif.optChild.get.toStr(x)))
  else:
    result.add("\n" & i & "optChild" & " " & "!isSome")
  result.add("\n" & iFinish & ")")
of astElse:
  result.add "(AstElse"
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myElse.stmtSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astSwitch:
  result.add "(AstSwitch"
  result.add(("\n" & i & "expr" & " ") & (ast.mySwitch.expr.toStr(x)))
  result.add(("\n" & i & "childSeq" & " ") & (ast.mySwitch.childSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astCase:
  result.add "(AstCase"
  result.add(("\n" & i & "expr" & " ") & (ast.myCase.expr.toStr(x)))
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myCase.stmtSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astDefault:
  result.add "(AstDefault"
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myDefault.stmtSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astFor:
  result.add "(AstFor"
  result.add(("\n" & i & "ident" & " ") & (ast.myFor.ident.toStr(x)))
  result.add(("\n" & i & "exprPre" & " ") & (ast.myFor.exprPre.toStr(x)))
  result.add(("\n" & i & "exprPost" & " ") & (ast.myFor.exprPost.toStr(x)))
  result.add(" " & $ast.myFor.isUntil)
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myFor.stmtSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astWhile:
  result.add "(AstWhile"
  result.add(("\n" & i & "expr" & " ") & (ast.myWhile.expr.toStr(x)))
  result.add(("\n" & i & "stmtSeq" & " ") & (ast.myWhile.stmtSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astContinue:
  result.add "(AstContinue"
  result.add(")")
of astBreak:
  result.add "(AstBreak"
  result.add(")")
of astReturn:
  result.add "(AstReturn"
  if ast.myReturn.optExpr.isSome:
    result.add(("\n" & i & "optExpr" & " ") &
        (ast.myReturn.optExpr.get.toStr(x)))
  else:
    result.add("\n" & i & "optExpr" & " " & "!isSome")
  result.add("\n" & iFinish & ")")
of astArray:
  result.add "(AstArray"
  result.add(("\n" & i & "dim" & " ") & (ast.myArray.dim.toStr(x)))
  result.add(("\n" & i & "elemType" & " ") & (ast.myArray.elemType.toStr(x)))
  result.add("\n" & iFinish & ")")
of astVoid:
  result.add "(AstVoid"
  result.add(")")
of astBool:
  result.add "(AstBool"
  result.add(")")
of astU8:
  result.add "(AstU8"
  result.add(")")
of astI8:
  result.add "(AstI8"
  result.add(")")
of astU16:
  result.add "(AstU16"
  result.add(")")
of astI16:
  result.add "(AstI16"
  result.add(")")
of astU32:
  result.add "(AstU32"
  result.add(")")
of astI32:
  result.add "(AstI32"
  result.add(")")
of astU64:
  result.add "(AstU64"
  result.add(")")
of astI64:
  result.add "(AstI64"
  result.add(")")
of astF32:
  result.add "(AstF32"
  result.add(")")
of astF64:
  result.add "(AstF64"
  result.add(")")
of astChar:
  result.add "(AstChar"
  result.add(")")
of astString:
  result.add "(AstString"
  result.add(")")
of astUnop:
  result.add "(AstUnop"
  result.add(" " & "kind" & " " & $ast.myUnop.kind)
  result.add(("\n" & i & "child" & " ") & (ast.myUnop.child.toStr(x)))
  result.add("\n" & iFinish & ")")
of astBinop:
  result.add "(AstBinop"
  result.add(" " & "kind" & " " & $ast.myBinop.kind)
  result.add(("\n" & i & "left" & " ") & (ast.myBinop.left.toStr(x)))
  result.add(("\n" & i & "right" & " ") & (ast.myBinop.right.toStr(x)))
  result.add("\n" & iFinish & ")")
of astAssignEtc:
  result.add "(AstAssignEtc"
  result.add(" " & "kind" & " " & $ast.myAssignEtc.kind)
  result.add(("\n" & i & "left" & " ") & (ast.myAssignEtc.left.toStr(x)))
  result.add(("\n" & i & "right" & " ") & (ast.myAssignEtc.right.toStr(x)))
  result.add("\n" & iFinish & ")")
of astNamedType:
  result.add "(AstNamedType"
  result.add(("\n" & i & "ident" & " ") & (ast.myNamedType.ident.toStr(x)))
  result.add(("\n" & i & "genericImplSeq" & " ") &
      (ast.myNamedType.genericImplSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astType:
  result.add "(AstType"
  result.add(("\n" & i & "childSeq" & " ") & (ast.myType.childSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
of astFuncCall:
  result.add "(AstFuncCall"
  result.add(("\n" & i & "ident" & " ") & (ast.myFuncCall.ident.toStr(x)))
  result.add(("\n" & i & "genericImplSeq" & " ") &
      (ast.myFuncCall.genericImplSeq.toStr(x)))
  result.add(("\n" & i & "argImplSeq" & " ") &
      (ast.myFuncCall.argImplSeq.toStr(x)))
  result.add("\n" & iFinish & ")")
