grammar scone;

exprLowestNonOp:
	TokIdent | /*literal |*/ '(' expr ')'
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
	'.' TokIdent
	;
//exprSuffixMethodCall:
//	'->' exprFuncCall
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
	// only `addr` allowed if `exprLhs`
	;

exprFieldArrEtc:
	exprLowestNonOp exprFieldArrEtcChoice?
	;
exprFieldArrEtcChoice:
	(
		exprSuffixFieldAccess
		//| exprSuffixMethodCall
		| exprSuffixDeref
		| exprSuffixArray
	)+
	;

exprLhsLowestNonOp:
	TokIdent | '(' exprLhs ')'
	;
exprLhs:
	exprLhsLowestNonOp exprFieldArrEtcChoice?
	;

TokIdent: 
	[_a-zA-Z][_a-zA-Z0-9]*
	;

options {
	k = 1;
}
