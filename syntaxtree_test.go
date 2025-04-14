package syntaxtree

import (
	"fmt"
	"regexp"
	"testing"
)

type Example struct {
	OperatorPrecedence []string
	OperatorParsers    []OperatorParser
	BinaryFunctions    []string
	UnaryFunctions     []string
}

func (e *Example) GetBinaryFunctionOperators(openingDelimiter byte, closingDelimiter byte, operatndSeparator byte) []BinaryFunctionParser {
	binaryFunctionParsers := make([]BinaryFunctionParser, len(e.BinaryFunctions))
	for i, binaryFunction := range e.BinaryFunctions {
		binaryFunctionParsers[i] = BinaryFunctionParser{
			FunctionName:     binaryFunction,
			OpeningDelimiter: openingDelimiter,
			ClosingDelimiter: closingDelimiter,
			OperandSeparator: operatndSeparator,
		}
	}

	return binaryFunctionParsers
}

func (e *Example) GetUnaryFunctionOperators(openingDelimiter byte, closingDelimiter byte) []UnaryFunctionParser {
	unaryFunctionParsers := make([]UnaryFunctionParser, len(e.UnaryFunctions))
	for i, unaryFunction := range e.UnaryFunctions {
		unaryFunctionParsers[i] = UnaryFunctionParser{
			FunctionName:     unaryFunction,
			OpeningDelimiter: openingDelimiter,
			ClosingDelimiter: closingDelimiter,
		}
	}

	return unaryFunctionParsers
}

func Equal[V comparable](t *testing.T, got, expected V) {
	t.Helper()

	if expected != got {
		t.Errorf(`Equal(
t,
got:
%v
,
expected:
%v
)`, got, expected)
	}
}

func Error(t *testing.T, err error) {
	t.Helper()

	if err == nil {
		t.Error("Expected err not to be nil but it is")
	}
}

func NoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Errorf("Expected error to be nil but it is not. err: %v", err)
	}
}

var (
	exampleMath = Example{
		OperatorPrecedence: []string{
			"pow",
			"sqrt",
			"/",
			"*",
			"+",
			"-",
		},
		OperatorParsers: []OperatorParser{
			{
				OperatorString:  "*",
				OperatorPattern: regexp.MustCompile(fmt.Sprintf(`([\d\(\)]*)\%s([\d\(\)]*|pow|sqrt)`, "*")),
			},
			{
				OperatorString:  "/",
				OperatorPattern: regexp.MustCompile(fmt.Sprintf(`([\d\(\)]*)\%s([\d\(\)]*|pow|sqrt)`, "/")),
			},
			{
				OperatorString:  "+",
				OperatorPattern: regexp.MustCompile(fmt.Sprintf(`([\d\(\)]*)\%s([\d\(\)]*|pow|sqrt)`, "+")),
			},
			{
				OperatorString:  "-",
				OperatorPattern: regexp.MustCompile(fmt.Sprintf(`([\d\(\)]*)\%s([\d\(\)]*|pow|sqrt)`, "-")),
			},
		},
		BinaryFunctions: []string{
			"pow",
		},
		UnaryFunctions: []string{
			"sqrt",
		},
	}

	exampleOdata = Example{
		OperatorPrecedence: []string{
			"length",
			"indexof",
			"tolower",
			"toupper",
			"trim",
			"year",
			"month",
			"day",
			"hour",
			"minute",
			"second",
			"fractionalsecond",
			"date",
			"time",
			"now",
			"round",
			"floor",
			"ceiling",
			"concat",
			"contains",
			"endswith",
			"startswith",
			"eq",
			"ne",
			"gt",
			"ge",
			"lt",
			"le",
			"and",
			"or",
		},
		OperatorParsers: []OperatorParser{
			{
				OperatorString:  "eq",
				OperatorPattern: regexp.MustCompile(`(.*?) eq (.*?)`),
			},
			{
				OperatorString:  "ne",
				OperatorPattern: regexp.MustCompile(`(.*?) ne (.*?)`),
			},
			{
				OperatorString:  "gt",
				OperatorPattern: regexp.MustCompile(`(.*?) gt (.*?)`),
			},
			{
				OperatorString:  "ge",
				OperatorPattern: regexp.MustCompile(`(.*?) ge (.*?)`),
			},
			{
				OperatorString:  "lt",
				OperatorPattern: regexp.MustCompile(`(.*?) lt (.*?)`),
			},
			{
				OperatorString:  "le",
				OperatorPattern: regexp.MustCompile(`(.*?) le (.*?)`),
			},
			{
				OperatorString:  "and",
				OperatorPattern: regexp.MustCompile(`(.*?) and (.*?)`),
			},
			{
				OperatorString:  "or",
				OperatorPattern: regexp.MustCompile(`(.*?) or (.*?)`),
			},
		},
		BinaryFunctions: []string{
			"concat",
			"contains",
			"endswith",
			"startswith",
		},
		UnaryFunctions: []string{
			"length",
			"indexof",
			"tolower",
			"toupper",
			"trim",
			"year",
			"month",
			"day",
			"hour",
			"minute",
			"second",
			"fractionalsecond",
			"date",
			"time",
			"now",
			"round",
			"floor",
			"ceiling",
		},
	}
)

func TestNodeTypeString_ReturnsCorrectValue(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		nodeType       NodeType
		expectedResult string
	}{
		"unknown": {
			nodeType:       Unknown,
			expectedResult: "Unknown",
		},
		"operator": {
			nodeType:       Operator,
			expectedResult: "Operator",
		},
		"unaryoperator": {
			nodeType:       UnaryOperator,
			expectedResult: "UnaryOperator",
		},
		"leftoperand": {
			nodeType:       LeftOperand,
			expectedResult: "LeftOperand",
		},
		"rightoperand": {
			nodeType:       RightOperand,
			expectedResult: "RightOperand",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Act
			result := testData.nodeType.String()

			// Assert
			Equal(t, result, testData.expectedResult)
		})
	}
}

func TestParseQuery_ReturnsError(t *testing.T) {
	// t.Parallel()
	tests := map[string]struct {
		syntaxTree       SyntaxTree
		query            string
		expectedErrorMsg string
	}{
		"missing closing bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "(",
			expectedErrorMsg: "failed to parse query: missing closing bracket ')'",
		},
		"missing opening bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "())",
			expectedErrorMsg: "failed to parse query: missing opening bracket '('",
		},
		// TODO: Fix these tests
		// "operator missing operand - 1": {
		// 	syntaxTree: SyntaxTree{
		// 		OperatorPrecedence:    exampleOdata.OperatorPrecedence,
		// 		OperatorParsers:       exampleOdata.OperatorParsers,
		// 		BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
		// 		UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
		// 		Separator:             ";",
		// 	},
		// 	query:            "name eq or name eq 'value'",
		// 	expectedErrorMsg: "?",
		// },
		// "operator missing operand - 2": {
		// 	syntaxTree: SyntaxTree{
		// 		OperatorPrecedence:    exampleOdata.OperatorPrecedence,
		// 		OperatorParsers:       exampleOdata.OperatorParsers,
		// 		BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
		// 		UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
		// 		Separator:             ";",
		// 	},
		// 	query:            "or name eq 'value'",
		// 	expectedErrorMsg: "?",
		// },
		// "operator missing operand - 3": {
		// 	syntaxTree: SyntaxTree{
		// 		OperatorPrecedence:    exampleOdata.OperatorPrecedence,
		// 		OperatorParsers:       exampleOdata.OperatorParsers,
		// 		BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
		// 		UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
		// 		Separator:             ";",
		// 	},
		// 	query:            "id or name eq 'value'",
		// 	expectedErrorMsg: "?",
		// },
		// "operator missing operand - 4": {
		// 	syntaxTree: SyntaxTree{
		// 		OperatorPrecedence:    exampleMath.OperatorPrecedence,
		// 		OperatorParsers:       exampleMath.OperatorParsers,
		// 		BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
		// 		UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
		// 		Separator:             ";",
		// 	},
		// 	query:            "*1-2",
		// 	expectedErrorMsg: "?",
		// },
		// "operator missing operand - 5": {
		// 	syntaxTree: SyntaxTree{
		// 		OperatorPrecedence:    exampleMath.OperatorPrecedence,
		// 		OperatorParsers:       exampleMath.OperatorParsers,
		// 		BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
		// 		UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
		// 		Separator:             ";",
		// 	},
		// 	query:            "2-",
		// 	expectedErrorMsg: "?",
		// },
		"operator missing operand - 6": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "name eq",
			expectedErrorMsg: "failed to parse query: possible typo in \"name eq\"",
		},
		"operator missing operand - 7": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "eq 'value'",
			expectedErrorMsg: "failed to parse query: possible typo in \"eq 'value'\"",
		},
		"binary function missing operand - 1": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "contains(name) or name eq 'value'",
			expectedErrorMsg: "failed to parse query: function 'contains' is missing an operand",
		},
		"binary function missing operand - 2": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "contains(name,'value') or concat(name) eq 'test'",
			expectedErrorMsg: "failed to parse query: function 'concat' is missing an operand",
		},
		"unary function missing operand - 1": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "length() ge 5",
			expectedErrorMsg: "failed to parse query: function 'length' is missing an operand",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			// t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			parsedQuery, err := syntaxTree.ParseQuery(query)

			// Assert
			Equal(t, parsedQuery, "")
			Error(t, err)
			if err != nil {
				Equal(t, err.Error(), testData.expectedErrorMsg)
			}
		})
	}
}

func TestParseQuery_ReturnsCorrectQuery(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree          SyntaxTree
		query               string
		expectedParsedQuery string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "1+2*3",
			expectedParsedQuery: "1;+;2;*;3",
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "(1+2)*3",
			expectedParsedQuery: "(;1;+;2;);*;3",
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "(1+2)*sqrt(3)",
			expectedParsedQuery: "(;1;+;2;);*;sqrt;(;3;)",
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "(1+2)*sqrt(pow(2,pow(3,3)))",
			expectedParsedQuery: "(;1;+;2;);*;sqrt;(;(;2;);pow;(;(;3;);pow;(;3;););)",
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3+3,pow(3,sqrt(2))))",
			expectedParsedQuery: "1;-;sqrt;(;(;2;);pow;(;3;);+;1;);*;2;/;(;sqrt;(;1;+;1;);*;(;3;+;3;);pow;(;(;3;);pow;(;sqrt;(;2;);););)",
		},
		"math complex example different delimiters": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('[', ']', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('{', '}'),
				Separator:             ";",
			},
			query:               "1-sqrt{pow[2,3]+1}*2/(sqrt{1+1}*pow[3+3,pow[3,sqrt{2}]])",
			expectedParsedQuery: "1;-;sqrt;(;(;2;);pow;(;3;);+;1;);*;2;/;(;sqrt;(;1;+;1;);*;(;3;+;3;);pow;(;(;3;);pow;(;sqrt;(;2;);););)",
		},
		"odata simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "toupper(tolower(name)) eq 'JOHN'",
			expectedParsedQuery: "toupper;(;tolower;(;name;););eq;'JOHN'",
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
			expectedParsedQuery: "name;eq;'John';and;(;(;lastname;);concat;(;(;' ';);concat;(; name;););eq;'Smith John';or;(;(;name;);concat;(;lastname;););contains;(;'Smith';);or;length;(;(;name;);concat;(;lastname;););eq;10;)",
		},
		"odata complex example 2": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10) and name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John'",
			expectedParsedQuery: "(;(;name;);concat;(;lastname;););contains;(;'Smith';);or;length;(;(;name;);concat;(;lastname;););eq;10;);and;name;eq;'John';and;(;(;lastname;);concat;(;(;' ';);concat;(; name;););eq;'Smith John'",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			parsedQuery, err := syntaxTree.ParseQuery(query)

			// Assert
			NoError(t, err)
			Equal(t, parsedQuery, testData.expectedParsedQuery)
		})
	}
}

func TestConstructTree_ReturnsError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree       SyntaxTree
		query            string
		expectedErrorMsg string
	}{
		"example missing opening bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "(1+2))*3",
			expectedErrorMsg: "failed to parse query: missing opening bracket '('",
		},
		"example missing closing bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "(1+(2*3)",
			expectedErrorMsg: "failed to parse query: missing closing bracket ')'",
		},
		"example parsing error typo last part": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "concat('#',name) qe '#test'",
			expectedErrorMsg: "failed to parse query: possible typo in \"( name ) qe '#test'\"",
		},
		"example parsing error typo first part": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "conct('#',name) eq '#test'",
			expectedErrorMsg: "failed to parse query: possible typo in \"conct( '#',name\"",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.ConstructTree(query)

			// Assert
			Error(t, err)
			Equal(t, err.Error(), testData.expectedErrorMsg)
		})
	}
}

func TestConstructTree_ReturnsNoError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree SyntaxTree
		query      string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "1+2*3",
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "(1+2)*3",
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(3)",
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(pow(2,pow(3,3)))",
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3,pow(3,2)))",
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.ConstructTree(query)

			// Assert
			NoError(t, err)
		})
	}
}

func TestConstructTree_CreatesCorrectGraph(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree    SyntaxTree
		query         string
		expectedGraph string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "1+2*3",
			expectedGraph: `graph {
	"1 [+]" -- "0 [1]"
	"3 [*]" -- "2 [2]"
	"3 [*]" -- "4 [3]"
	"1 [+]" -- "3 [*]"
}`,
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "(1+2)*3",
			expectedGraph: `graph {
	"1 [+]" -- "0 [1]"
	"1 [+]" -- "2 [2]"
	"3 [*]" -- "1 [+]"
	"3 [*]" -- "4 [3]"
}`,
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(3)",
			expectedGraph: `graph {
	"1 [+]" -- "0 [1]"
	"1 [+]" -- "2 [2]"
	"3 [*]" -- "1 [+]"
	"4 [sqrt]" -- "5 [3]"
	"3 [*]" -- "4 [sqrt]"
}`,
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(pow(2,pow(3,sqrt(3))))",
			expectedGraph: `graph {
	"1 [+]" -- "0 [1]"
	"1 [+]" -- "2 [2]"
	"3 [*]" -- "1 [+]"
	"6 [pow]" -- "5 [2]"
	"8 [pow]" -- "7 [3]"
	"9 [sqrt]" -- "10 [3]"
	"8 [pow]" -- "9 [sqrt]"
	"6 [pow]" -- "8 [pow]"
	"4 [sqrt]" -- "6 [pow]"
	"3 [*]" -- "4 [sqrt]"
}`,
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3+3,pow(3,sqrt(2))))",
			expectedGraph: `graph {
	"1 [-]" -- "0 [1]"
	"4 [pow]" -- "3 [2]"
	"4 [pow]" -- "5 [3]"
	"6 [+]" -- "4 [pow]"
	"6 [+]" -- "7 [1]"
	"2 [sqrt]" -- "6 [+]"
	"8 [*]" -- "2 [sqrt]"
	"10 [/]" -- "9 [2]"
	"13 [+]" -- "12 [1]"
	"13 [+]" -- "14 [1]"
	"11 [sqrt]" -- "13 [+]"
	"15 [*]" -- "11 [sqrt]"
	"17 [+]" -- "16 [3]"
	"17 [+]" -- "18 [3]"
	"19 [pow]" -- "17 [+]"
	"21 [pow]" -- "20 [3]"
	"22 [sqrt]" -- "23 [2]"
	"21 [pow]" -- "22 [sqrt]"
	"19 [pow]" -- "21 [pow]"
	"15 [*]" -- "19 [pow]"
	"10 [/]" -- "15 [*]"
	"8 [*]" -- "10 [/]"
	"1 [-]" -- "8 [*]"
}`,
		},
		"odata simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "toupper(tolower(name)) eq 'JOHN'",
			expectedGraph: `graph {
	"1 [tolower]" -- "2 [name]"
	"0 [toupper]" -- "1 [tolower]"
	"3 [eq]" -- "0 [toupper]"
	"3 [eq]" -- "4 ['JOHN']"
}`,
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             "|",
			},
			query: "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
			expectedGraph: `graph {
	"1 [eq]" -- "0 [name]"
	"1 [eq]" -- "2 ['John']"
	"3 [and]" -- "1 [eq]"
	"5 [concat]" -- "4 [lastname]"
	"7 [concat]" -- "6 [' ']"
	"7 [concat]" -- "8 [ name]"
	"5 [concat]" -- "7 [concat]"
	"9 [eq]" -- "5 [concat]"
	"9 [eq]" -- "10 ['Smith John']"
	"11 [or]" -- "9 [eq]"
	"13 [concat]" -- "12 [name]"
	"13 [concat]" -- "14 [lastname]"
	"15 [contains]" -- "13 [concat]"
	"15 [contains]" -- "16 ['Smith']"
	"11 [or]" -- "15 [contains]"
	"17 [or]" -- "11 [or]"
	"20 [concat]" -- "19 [name]"
	"20 [concat]" -- "21 [lastname]"
	"18 [length]" -- "20 [concat]"
	"22 [eq]" -- "18 [length]"
	"22 [eq]" -- "23 [10]"
	"17 [or]" -- "22 [eq]"
	"3 [and]" -- "17 [or]"
}`,
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.ConstructTree(query)

			// Assert
			NoError(t, err)
			Equal(t, syntaxTree.String(), testData.expectedGraph)
		})
	}
}
