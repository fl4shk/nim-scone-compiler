module:
	'module' ident '('
		(
			funcDecl
			| structDecl
			//| macroDecl
			//| variantDecl
			//| tupleDecl
			| constDecl
			| externDecl
			| importDecl
		)*
	')'
	;

funcDecl:
	'def' ident
	( '{' genericDecl+ '}' )?
	'('
		funcDeclArgList
	')' '('
		stmtList
	')' ';'
	;

funcDeclArgList:
	( identList ':' typeWithOptPreKwVar ',' )* 
	'result' typeWithoutOptPreKwVar
	;

identList:
	(ident (',' ident)*)+
	;


varEtcDeclMost:
	identList ':' typeWithOptPreKwVar
	;

//--------
varDecl:
	'var' varEtcDeclMost ('=' expr)?
	;
letDecl:
	'let' varEtcDeclMost '=' expr
	;
constDecl:
	'const' varEtcDeclMost '=' expr
	;
//--------
stmt:
	varDecl | letDecl | constDecl
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
	'return' 
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
	ident | literal | '(' expr ')'
	;

expr:
	exprLowestNonOp
	| exprLogicOr // the lowest precedence operator
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

exprSuffixFieldAccess:
	'.' ident
	;
exprSuffixMethodCall:
	'->' exprFuncCall
	;

exprSuffixDeref:
	'[]'
	;

exprSuffixArray:
	'[' expr ']'
	;

exprPrefixUnary:
	'+' | '-' | '!' | '~'

	| 'addr'
	// only `addr` allowed if `exprLhs`
	;

exprFieldArrEtc:
	exprLowestNonOp exprFieldArrEtcChoice?
	;
exprFieldArrEtcChoice:
	(
		exprSuffixFieldAccess
		| exprSuffixMethodCall
		| exprSuffixDeref
		| exprSuffixArray
	)+
	;

exprLhsLowestNonOp:
	ident | '(' exprLhs ')'
	;
exprLhs:
	exprLhsLowestNonOp exprFieldArrEtcChoice?
	;


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

	;

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
	;


typeWithoutOptPreKwVar:
	('ptr')* typeMain
typeWithOptPreKwVar:
	('var' | ('ptr')+ )? typeMain
	;

typeToResolve:
	ident ( '{' genericImpl '}' )?
	;

typeBuiltinScalar:
	'u8' | 'u16' | 'u32' | 'u64'
	| 'i8' | 'i16' | 'i32' | 'i64'
	| 'f32' | 'f64'
	| 'string' | 'char'
	| 'void'
	;

genericImpl:
	ident '=' typeWithOptPreKwVar
	// `var` will simply be ignored if this `genericImpl` is for a `struct`
	// field
	;

structDecl:
	'struct' ident
	( '{' genericDecl+ '}' )?
	'('
		// perhaps support methods in this `structDecl` rule later?
		// Or alternatively do something like Nim does and have syntax
		// sugar when the first argument to a function is of the same type
		// as the struct on which to execute the method.
		// I like that approach, so that's what I'll do for now!
		( varEtcDeclMost ';' )*
	')'
	;

//stmtList:
