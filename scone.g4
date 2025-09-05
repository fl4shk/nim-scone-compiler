grammar scone;

srcFile:
	module
	(
		funcDecl
		| structDecl
		//| enumDecl
		//| macroDecl
		//| variantDecl
		//| tupleDecl
		//| stmtConstDecl
		//| externDecl
		//| importDecl
	)*
	;

module:
	'module' ident ';'
	;

funcDecl:
	'def' ident
	( '[' genericDeclList ']' )?
	'(' funcArgDeclList? ')' '->' typeWithOptPreKwVar '{'
		//stmtList
		stmt*
	'}' ';'
	;

funcArgDeclList:
	( ident ':' typeWithOptPreKwVar ',' )+
	//'result' ':' type //typeWithoutOptPreKwVar
	;

funcNamedArgImplList:
	//'$' 
	funcNamedArgImplItem (',' funcNamedArgImplItem )* ',' ?
	//| expr (',' expr)* (',') ?
	//| expr
	;

funcNamedArgImplItem:
	ident '=' expr
	;

funcUnnamedArgImplList:
	expr (',' expr)*
	;

structDecl:
	'struct' ident
	( '[' genericDeclList ']' )?
	'{'
		( varEtcDeclMost ';' )*
	'}'
	;

//identList:
//	(ident (',' ident)* )+
//	;


varEtcDeclMost: // "Most" is short for "Most of it"
	ident ':' typeWithoutOptPreKwVar
	;

//--------
stmtVarDecl:
	'var' varEtcDeclMost ('=' expr)? ';'
	;
//letDecl:
//	'let' varEtcDeclMost '=' expr ';'
//	;
stmtConstDecl:
	'const' varEtcDeclMost '=' expr ';'
	;
//--------
stmt:
	stmrVarDecl /*| letDecl*/ | stmtConstDecl
	| stmtBreak | stmtContinue
	| stmtFor | stmtWhile
	| stmtIf | stmtSwitch 
	| stmtScope
	| stmtReturn
	| stmtCallOrAssignEtc
	;
	
//stmtList:
//	stmt* 
//	;

stmtBreak:
	'break' ';'
	;
stmtContinue:
	'continue' ';'
	;

stmtFor:
	'for' ident 'in' expr ('to' | 'until') expr '{'
		//stmtList
		stmt*
	'}'
	;

stmtWhile:
	'while' expr '{'
		//stmtList
		stmt*
	'}'
	;

stmtIf:
	'if' expr '{'
		//stmtList
		stmt*
	'}'
	stmtElif*
	stmtElse?
	;
stmtElif:
	'elif' expr '{'
		//stmtList
		stmt*
	'}'
	;
stmtElse:
	'else' '{'
		//stmtList
		stmt*
	'}'
	;

stmtSwitch:
	'switch' expr '{'
		stmtCase*
		stmtDefault?
	'}'
	;
stmtCase:
	'case' expr '{'
		//stmtList
		stmt*
	'}'
	;
stmtDefault:
	'default' '{'
		//stmtList
		stmt*
	'}'
	;

stmtScope:
	'scope' '{'
		//stmtList
		stmt*
	'}'
	;

stmtReturn:
	'return' expr? ';'
	;

assignOp:
	'='
	| '+=' | '-='
	| '*=' | '/=' | '%='
	| '&=' | '|=' | '^='
	| '<<=' | '>>='
	;

stmtCallOrAssignEtc:
	exprLhs
	(
		assignOp
		expr
	)?
	';'
	;
//--------
exprLowestNonOp:
	//exprIdentOrFuncCall
	(
		//ident exprFuncCallPostIdent?
		exprIdentOrFuncCall
		| literal //exprFuncCall?
		| '(' expr ')' //exprFuncCall?
	)
	//exprFuncCallPostIdent?
	;

//exprList:
//	expr (',' expr)* ','
//	;

expr:
	//exprLowestNonOp
	//| 
	exprLogicOr // the lowest precedence operator
	;

exprLogicOr:
	exprLogicAnd ('||' exprLogicAnd)*
	;
exprLogicAnd:
	exprBitOr ('&&' exprBitOr)*
	;
exprBitOr:
	exprBitXor ('|' exprBitXor)*
	;
exprBitXor:
	exprBitAnd ('^' exprBitAnd)*
	;
exprBitAnd:
	exprCmpEqNe ('&' exprCmpEqNe)*
	;
exprCmpEqNe:
	exprCmpIneq (('==' | '!=') exprCmpIneq)*
	;
exprCmpIneq:
	//exprCmpEqNe
	exprBitShift (('<' | '<=' | '>' | '>=') exprBitShift)*
	;
exprBitShift:
	exprAddSub (('<<' | '>>') exprAddSub)*
	;
exprAddSub:
	exprMulDivMod (('+' | '-') exprMulDivMod)*
	;
exprMulDivMod:
	exprUnary (('*' | '/' | '%') exprUnary)*
	//exprBinopFuncCall (('*' | '/' | '%') exprBinopFuncCall)*
	;

//exprBinopFuncCall:
//	exprUnary (ident genericFullImplList? exprUnary)*
//
//	//exprLowestNonOp
//	//expr
//	//exprFieldArrEtcChoice
//	;
//exprAddrLhsMain:
//	'addr' exprLhsMain
//	;
//exprOptPrefixUnaryMain:
//	exprPrefixUnary? exprFieldArrEtc
//	;

exprUnary:
	//(
	//	exprPrefixUnary? exprFieldArrEtc
	//	| 'addr' exprLhsMain
	//)
	//exprOptPrefixUnaryMain
	//| exprAddrLhsMain
	exprPrefixUnary? exprFieldArrEtc
	;

exprSuffixFieldMethodAccessDotExpr:
	//'.' exprIdentOrFuncCall
	'.' exprIdentOrFuncCallPostDot
	;
exprSuffixFieldMethodAccess:
	exprSuffixFieldMethodAccessDotExpr
	//| exprBinopFuncCall
	;
//exprSuffixMethodCall:
//	'->' exprFuncCallMain
//	;

exprSuffixDeref:
	//'[]'
	'@'
	;

//exprSuffixArray:
//	'[' expr ']'
//	;

exprPrefixUnary:
	'+' | '-' | '!' | '~'

	| 'addr'
	;

exprFieldArrEtc:
	exprLowestNonOp exprFieldArrEtcChoice*
	;
exprFieldArrEtcChoice:
	exprSuffixFieldMethodAccess
	//| exprSuffixMethodCall
	| exprSuffixDeref
	//| exprSuffixArray
	//| exprFuncCall
	;

exprLhsLowestNonOpEtc:
	'addr' ?
	(
		//ident exprFuncCallPostIdent?
		exprIdentOrFuncCall
		| '(' exprLhs ')'
	)
	;

//exprLhsMain:
//	exprLhsLowestNonOpEtc exprFieldArrEtcChoice*
//	;
exprLhs:
	//'addr'? exprLhsMain
	//'addr'? 
	(
		exprLhsLowestNonOpEtc exprFieldArrEtcChoice*
	)
	;

exprIdentOrFuncCall:
	ident

	exprFuncCallPostIdent?	// if we have `exprFuncCallPostIdent`,
							// this indicates calling either 
							// a function or method
	;

exprIdentOrFuncCallPostDot:
	ident

	exprFuncCallPostIdent?	// if we have `exprFuncCallPostIdent`,
							// this indicates calling either 
							// a function or method
	;

//exprFuncCall:
//	ident exprFuncCallPostIdent
//	;

exprFuncCallPostIdent:
	//( '[' genericImplList ']' )? 
	genericFullImplList?
	exprFuncCallPostGeneric
	;

//exprBinopFuncCall:
//	ident
//	genericFullImplList?
//	exprLowestNonOp
//	//expr
//	//exprFieldArrEtcChoice
//	;

//exprFuncCallPostGenericBinop:
//	ident
//	;
exprFuncCallPostGeneric:
	//'$(' funcNamedArgImplList? ')'
	//| '(' funcUnnamedArgImplList? ')'
	exprFuncCallPostGenericMain
	//| expr
	;
exprFuncCallPostGenericMain:
	'(' 
		(
			funcUnnamedArgImplList (',' funcNamedArgImplList?)?
			| funcNamedArgImplList
		)?
	')'
	;
	
//--------

typeMain:
	typeBasicBuiltin
	| typeToResolve
	| typeArray
	//| 'array' '[' expr (',' expr)* ':' typeWithoutOptPreKwVar ']'
	//| 'array' '{'
	//	('dim' '=')? expr ','
	//	('ElemT' '=')? typeWithoutOptPreKwVar
	//'}'
	;

typeArray:
	'array' '[' expr ';' typeWithoutOptPreKwVar ']'
	;


typeWithoutOptPreKwVar:
	('ptr')* typeMain //typeArrDim*
	;
typeWithOptPreKwVar:
	('var' | 'ptr'+ )?
	(
		typeMain //typeArrDim*
	)
	;

typeToResolve:
	ident
	(
		//'[' genericImplList ']' 
		genericFullImplList
	)?
	;

typeBasicBuiltin:
	'u8' | 'u16' | 'u32' | 'u64'
	| 'i8' | 'i16' | 'i32' | 'i64'
	| 'f32' | 'f64'
	| 'string' | 'char'
	| 'void'
	;

genericDeclList:
	genericDeclItem ( ',' genericDeclItem )* ',' ?
	//identList
	;

genericDeclItem:
	ident
	;

genericFullImplList:
	(
		//'$[' genericNamedImplList 
		//| '[' genericUnnamedImplList
		'['
			(
				genericUnnamedImplList (',' genericNamedImplList?)?
				| genericNamedImplList
			)
		']'
	)
	//']'
	;

genericNamedImplList:
	genericNamedImplItem (',' genericNamedImplItem)* ',' ?
	;

genericNamedImplItem:
	ident '=' typeWithoutOptPreKwVar
	;
genericUnnamedImplList:
	typeWithoutOptPreKwVar (',' typeWithoutOptPreKwVar)* //',' ?
	;

ident:
	TokIdent
	;

TokIdent:
	[_a-zA-Z][_a-zA-Z0-9]*
	;

//stmtList:
