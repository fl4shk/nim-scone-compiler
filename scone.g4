grammar scone;

srcFile:
	module
	(
		funcDecl
		| structDecl
		//| macroDecl
		//| variantDecl
		//| tupleDecl
		//| constDecl ';'
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
		stmtList
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
varDecl:
	'var' varEtcDeclMost ('=' expr)?
	;
letDecl:
	'let' varEtcDeclMost '=' expr
	;
//constDecl:
//	'const' varEtcDeclMost '=' expr
//	;
//--------
stmt:
	varDecl | letDecl //| constDecl
	| breakStmt | continueStmt
	| forStmt | whileStmt
	| ifStmt
	| returnStmt
	| assignStmt
	;
	
stmtList:
	(stmt ';')* 
	;

breakStmt:
	'break'
	;
continueStmt:
	'continue'
	;

forStmt:
	'for' '(' ident 'in' expr ('to' | 'until') expr ')' '{'
		stmtList
	'}'
	;

whileStmt:
	'while' '(' expr ')' '{'
		stmtList
	'}'
	;

ifStmt:
	'if' '(' expr ')' '{'
		stmtList
	'}'
	elifStmt*
	elseStmt?
	;
elifStmt:
	'elif' '(' expr ')' '{'
		stmtList
	'}'
	;
elseStmt:
	'else' '{'
		stmtList
	'}'
	;

switchStmt:
	'switch' '('
		expr
	')' '{'
		caseStmt*
		defaultStmt?
	'}'
	;
caseStmt:
	'case' '(' expr ')' '{'
		stmtList
	'}'
	;
defaultStmt:
	'default' '{'
		stmtList
	'}'
	;

returnStmt:
	'return' expr?
	;

assignStmt:
	exprLhs
	(
		'='
		| '+=' | '-='
		| '*=' | '/=' | '%='
		| '&=' | '|=' | '^='
		| '<<=' | '>>='
	)
	expr
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
	exprLogicAnd ('||' expr)?
	;
exprLogicAnd:
	exprBitOr ('&&' expr)?
	;
exprBitOr:
	exprBitXor ('|' expr)?
	;
exprBitXor:
	exprBitAnd ('^' expr)?
	;
exprBitAnd:
	exprCmpEqNe ('&' expr)?
	;
exprCmpEqNe:
	exprCmpIneq (('==' | '!=') expr)?
	;
exprCmpIneq:
	//exprCmpEqNe
	exprBitShift (('<' | '<=' | '>' | '>=') expr)?
	;
exprBitShift:
	exprAddSub (('<<' | '>>') expr)?
	;
exprAddSub:
	exprMulDivMod (('+' | '-') expr)?
	;
exprMulDivMod:
	exprUnary (('*' | '/' | '%') expr)?
	;

exprUnary:
	exprPrefixUnary? exprFieldArrEtc
	;

exprSuffixFieldMethodAccessDotExpr:
	'.' exprIdentOrFuncCall
	;
exprSuffixFieldMethodAccess:
	exprSuffixFieldMethodAccessDotExpr
	| exprBinopFuncCall
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

exprLhs:
	exprLhsLowestNonOpEtc exprFieldArrEtcChoice*
	;

exprIdentOrFuncCall:
	//'$' exprFuncCallMain
	// '$' (or some other leading token) required because the parser needs
	// to have at most one token to determine if a rule can be taken.
	ident

	exprFuncCallPostIdent?	// if we have `exprFuncCallPostIdent`,
							// this indicates calling either 
							// a function or method
	;
exprFuncCall:
	ident exprFuncCallPostIdent
	;

exprFuncCallPostIdent:
	//( '[' genericImplList ']' )? 
	genericFullImplList?
	exprFuncCallPostGeneric
	;

exprBinopFuncCall:
	ident
	genericFullImplList?
	expr
	;

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
	typeBuiltinScalar
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

typeBuiltinScalar:
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
