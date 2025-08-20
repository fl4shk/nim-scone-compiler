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
	( '{' genericDeclList '}' )?
	'(' funcArgDeclList ')' '('
		stmtList
	')' ';'
	;

funcArgDeclList:
	( identList ':' typeWithOptPreKwVar ',' )* 
	'result' ':' typeWithOptPreKwVar //typeWithoutOptPreKwVar
	;

funcArgImplList:
	funcArgImplItem ( ',' funcArgImplItem )* ',' ?
	//| expr
	;

funcArgImplItem:
	ident '=' expr
	;

structDecl:
	'struct' ident
	( '{' genericDeclList '}' )?
	'('
		( varEtcDeclMost ';' )*
	')'
	;

identList:
	(ident (',' ident)* )+
	;


varEtcDeclMost: // "Most" is short for "Most of it"
	identList ':' typeWithOptPreKwVar
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
	'for' '(' ident 'in' expr ('to' | 'until') expr ')' '('
		stmtList
	')'
	;

whileStmt:
	'while' '(' expr ')' '('
		stmtList
	')'
	;

ifStmt:
	'if' '(' expr ')' '('
		stmtList
	')'
	elifStmt*
	elseStmt?
	;
elifStmt:
	'elif' '(' expr ')' '('
		stmtList
	')'
	;
elseStmt:
	'else' '('
		stmtList
	')'
	;

switchStmt:
	'switch' '('
		expr
	')' '('
		caseStmt*
		defaultStmt?
	')'
	;
caseStmt:
	'case' '(' expr ')' '('
		stmtList
	')'
	;
defaultStmt:
	'default' '('
		stmtList
	')'
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
	exprIdentOrFuncCall | /*literal |*/ '(' expr ')'
	;

expr:
	//exprLowestNonOp
	//| 
	exprLogicOr // the lowest precedence operator
	;

exprLogicOr:
	exprLogicAnd
	(
		'||'
		expr
	)?
	;
exprLogicAnd:
	exprBitOr
	(
		'&&'
		expr
	)?
	;
exprBitOr:
	exprBitXor
	(
		'|'
		expr
	)?
	;
exprBitXor:
	exprBitAnd
	(
		'^'
		expr
	)?
	;
exprBitAnd:
	exprCmpEqNe
	(
		'&'
		expr
	)?
	;
exprCmpEqNe:
	exprCmpIneq
	(
		('==' | '!=')
		expr
	)?
	;
exprCmpIneq:
	//exprCmpEqNe
	exprBitShift
	(
		('<' | '<=' | '>' | '>=')
		expr
	)?
	;
exprBitShift:
	exprAddSub
	(
		('<<' | '>>')
		expr
	)?
	;
exprAddSub:
	exprMulDivMod
	(
		('+' | '-')
		expr
	)?
	;
exprMulDivMod:
	exprUnary
	(
		('*' | '/' | '%')
		expr
	)?
	;

exprUnary:
	exprPrefixUnary? exprFieldArrEtc
	;

exprSuffixFieldMethodAccess:
	'.' exprIdentOrFuncCall
	;
//exprSuffixMethodCall:
//	'->' exprFuncCallMain
//	;

exprSuffixDeref:
	'[]'
	;

exprSuffixArray:
	'[' expr ']'
	;

exprPrefixUnary:
	'+' | '-' | '!' | '~'

	| 'addr'
	;

exprFieldArrEtc:
	exprLowestNonOp exprFieldArrEtcChoice*
	;
exprFieldArrEtcChoice:
	exprSuffixFieldAccess
	//| exprSuffixMethodCall
	| exprSuffixDeref
	| exprSuffixArray
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
exprFuncCallPostIdent:
	( '{' genericImplList '}' )? 
	'('
		funcArgImplList?
	')'
	;

//exprFuncCallMain:
//	ident ( '{' genericImplList '}' )? 
//	'('
//		funcArgImplList?
//	')'
//	;


//exprFieldArrEtc:
//	(
//		exprPrefixUnary expr
//	) | (
//		exprPrio1
//		(
//			exprSuffixFieldAccess
//			| exprSuffixMethodCall
//			| exprSuffixDeref
//			| exprSuffixArray
//		)*
//	)
//	;
//--------

//	;

//expr:
//	exprPrio1
//	(
//		(
//			'.' ident // struct field access
//			'.@' exprFuncCall
//		)
//		expr
//	)?
//	;
//expr:
//	exprPrio1
//	(
//		(
//			'.' ident				// struct field access
//			| '[]'					// pointer dereference
//			| ( '[' expr ']' )	// array access
//			| ( '@' exprFuncCall )	// function call
//			//| ( '$' exprMacroCall	) // macro call (add this later)
//		)
//		expr
//	)?
//	;
//
//exprPrio1:
//	exprPrio2
//	(
//		expr
//	)?
//	;

//--------

typeMain:
	typeBuiltinScalar
	| typeToResolve
	//| 'array' '{'
	//	('dim' '=')? expr ','
	//	('ElemT' '=')? typeWithoutOptPreKwVar
	//'}'
	;


typeArrDim:
	'[' expr ']'
	;

//typeWithoutOptPreKwVar:
//	('ptr')* typeMain typeArrDim*
typeWithOptPreKwVar:
	('var' | 'ptr'+ )? typeMain typeArrDim*
	;

typeToResolve:
	ident ( '{' genericImplList '}' )?
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

genericImplList:
	genericImplItem ( ',' genericImplItem )* ',' ?
	;

genericImplItem:
	ident '=' typeWithOptPreKwVar
	// `var` will simply be ignored if this `genericImplList` is for a
	// `struct` field
	;

ident:
	TokIdent
	;

TokIdent:
	[_a-zA-Z][_a-zA-Z0-9]*
	;

//stmtList:
