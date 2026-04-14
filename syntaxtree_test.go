package syntaxtree

import (
	"testing"
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
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Act
			result := testData.nodeType.String()

			// Assert
			Equal(t, result, testData.expectedResult)
		})
	}
}

func TestBuildTree_ReturnsError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree       SyntaxTree
		query            string
		expectedErrorMsg string
	}{
		"example missing lexer": {
			syntaxTree:       SyntaxTree{},
			query:            "(1+2)*3",
			expectedErrorMsg: "failed to parse query: no lexer defined, cannot tokenize the query",
		},
		"example empty query": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query:            "",
			expectedErrorMsg: "failed to parse query: unexpected token: \"\" (Unknown)",
		},
		"example missing opening bracket": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query:            "(1+2))*3",
			expectedErrorMsg: "failed to parse query: unexpected \")\" without matching opening bracket",
		},
		"example missing closing bracket": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query:            "(1+(2*3)",
			expectedErrorMsg: "failed to parse query: expected closing bracket but got \"\"",
		},
		"example parsing error typo last part": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query:            "concat('#',name) qe '#test'",
			expectedErrorMsg: "failed to parse query: unexpected token \"qe'#test'\" (StringOperand) after \"concat\" (Operator)",
		},
		"example parsing error typo first part": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query:            "conct('#',name) eq '#test'",
			expectedErrorMsg: "failed to parse query: unexpected token \"(\" (OpenDelimiter) after \"conct\" (LeftOperand)",
		},
	}

	for name, testData := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.BuildTree(query)

			// Assert
			Error(t, err)
			Equal(t, err.Error(), testData.expectedErrorMsg)
		})
	}
}

func TestBuildTree_CreatesCorrectGraph(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree    SyntaxTree
		query         string
		expectedGraph string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query: "1+2*3",
			expectedGraph: `graph {
	"4 [+]" -- "0 [1]"
	"3 [*]" -- "1 [2]"
	"3 [*]" -- "2 [3]"
	"4 [+]" -- "3 [*]"
}`,
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query: "(1+2)*3",
			expectedGraph: `graph {
	"2 [+]" -- "0 [1]"
	"2 [+]" -- "1 [2]"
	"4 [*]" -- "2 [+]"
	"4 [*]" -- "3 [3]"
}`,
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query: "(1+2)*sqrt(3)",
			expectedGraph: `graph {
	"2 [+]" -- "0 [1]"
	"2 [+]" -- "1 [2]"
	"5 [*]" -- "2 [+]"
	"4 [sqrt]" -- "3 [3]"
	"5 [*]" -- "4 [sqrt]"
}`,
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query: "(1+2)*sqrt(pow(2,pow(3,sqrt(3))))",
			expectedGraph: `graph {
	"2 [+]" -- "0 [1]"
	"2 [+]" -- "1 [2]"
	"10 [*]" -- "2 [+]"
	"8 [pow]" -- "3 [2]"
	"7 [pow]" -- "4 [3]"
	"6 [sqrt]" -- "5 [3]"
	"7 [pow]" -- "6 [sqrt]"
	"8 [pow]" -- "7 [pow]"
	"9 [sqrt]" -- "8 [pow]"
	"10 [*]" -- "9 [sqrt]"
}`,
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query: "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3+3,pow(3,sqrt(2))))",
			expectedGraph: `graph {
	"23 [-]" -- "0 [1]"
	"3 [pow]" -- "1 [2]"
	"3 [pow]" -- "2 [3]"
	"5 [+]" -- "3 [pow]"
	"5 [+]" -- "4 [1]"
	"6 [sqrt]" -- "5 [+]"
	"8 [*]" -- "6 [sqrt]"
	"8 [*]" -- "7 [2]"
	"22 [/]" -- "8 [*]"
	"11 [+]" -- "9 [1]"
	"11 [+]" -- "10 [1]"
	"12 [sqrt]" -- "11 [+]"
	"21 [*]" -- "12 [sqrt]"
	"15 [+]" -- "13 [3]"
	"15 [+]" -- "14 [3]"
	"20 [pow]" -- "15 [+]"
	"19 [pow]" -- "16 [3]"
	"18 [sqrt]" -- "17 [2]"
	"19 [pow]" -- "18 [sqrt]"
	"20 [pow]" -- "19 [pow]"
	"21 [*]" -- "20 [pow]"
	"22 [/]" -- "21 [*]"
	"23 [-]" -- "22 [/]"
}`,
		},
		"odata simple example": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query: "toupper(tolower(name)) eq 'JOHN'",
			expectedGraph: `graph {
	"1 [tolower]" -- "0 [name]"
	"2 [toupper]" -- "1 [tolower]"
	"4 [eq]" -- "2 [toupper]"
	"4 [eq]" -- "3 ['JOHN']"
}`,
		},
		"odata simple example multibyte string": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query: "contains(tolower(name),'café')",
			expectedGraph: `graph {
	"1 [tolower]" -- "0 [name]"
	"3 [contains]" -- "1 [tolower]"
	"3 [contains]" -- "2 ['café']"
}`,
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query: "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
			expectedGraph: `graph {
	"2 [eq]" -- "0 [name]"
	"2 [eq]" -- "1 ['John']"
	"23 [and]" -- "2 [eq]"
	"7 [concat]" -- "3 [lastname]"
	"6 [concat]" -- "4 [' ']"
	"6 [concat]" -- "5 [name]"
	"7 [concat]" -- "6 [concat]"
	"9 [eq]" -- "7 [concat]"
	"9 [eq]" -- "8 ['Smith John']"
	"15 [or]" -- "9 [eq]"
	"12 [concat]" -- "10 [name]"
	"12 [concat]" -- "11 [lastname]"
	"14 [contains]" -- "12 [concat]"
	"14 [contains]" -- "13 ['Smith']"
	"15 [or]" -- "14 [contains]"
	"22 [or]" -- "15 [or]"
	"18 [concat]" -- "16 [name]"
	"18 [concat]" -- "17 [lastname]"
	"19 [length]" -- "18 [concat]"
	"21 [eq]" -- "19 [length]"
	"21 [eq]" -- "20 [10]"
	"22 [or]" -- "21 [eq]"
	"23 [and]" -- "22 [or]"
}`,
		},
		"odata complex example 2": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query: "not(contains(tolower(testValue),' ') and endswith(metadata/name,'prd')) and not(name eq 'test' or startswith(name,'prd'))",
			expectedGraph: `graph {
	"1 [tolower]" -- "0 [testValue]"
	"3 [contains]" -- "1 [tolower]"
	"3 [contains]" -- "2 [' ']"
	"7 [and]" -- "3 [contains]"
	"6 [endswith]" -- "4 [metadata/name]"
	"6 [endswith]" -- "5 ['prd']"
	"7 [and]" -- "6 [endswith]"
	"8 [not]" -- "7 [and]"
	"17 [and]" -- "8 [not]"
	"11 [eq]" -- "9 [name]"
	"11 [eq]" -- "10 ['test']"
	"15 [or]" -- "11 [eq]"
	"14 [startswith]" -- "12 [name]"
	"14 [startswith]" -- "13 ['prd']"
	"15 [or]" -- "14 [startswith]"
	"16 [not]" -- "15 [or]"
	"17 [and]" -- "16 [not]"
}`,
		},
	}

	for name, testData := range tests {
		t.Run(name, func(t *testing.T) {
			// t.Parallel()
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.BuildTree(query)

			// Assert
			NoError(t, err)
			Equal(t, syntaxTree.String(), testData.expectedGraph)
		})
	}
}

// WARNING: Deprecated
func TestParseQuery_ReturnsError(t *testing.T) {
	t.Parallel()
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
		"operator missing operand - 1": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "*1-2",
			expectedErrorMsg: "failed to parse query: operator '*' does not have a left operand",
		},
		"operator missing operand - 2": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:            "2-",
			expectedErrorMsg: "failed to parse query: operator '-' does not have a right operand",
		},
		"operator missing operand - 3": {
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
		"operator missing operand - 4": {
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
		t.Run(name, func(t *testing.T) {
			t.Parallel()
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

// WARNING: Deprecated
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
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators('(', ')'),
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
		"odata simple example multibyte string": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query:               "contains(tolower(name),'café')",
			expectedParsedQuery: "(;tolower;(;name;););contains;(;'café';)",
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

// WARNING: Deprecated
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

// WARNING: Deprecated
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

// WARNING: Deprecated
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
		"odata simple example multibyte string": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators('(', ')', ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators('(', ')'),
				Separator:             ";",
			},
			query: "contains(tolower(name),'café')",
			expectedGraph: `graph {
	"0 [tolower]" -- "1 [name]"
	"2 [contains]" -- "0 [tolower]"
	"2 [contains]" -- "3 ['café']"
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
