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
