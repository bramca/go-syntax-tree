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
			nodeType:       UnaryFunction,
			expectedResult: "UnaryFunction",
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

// TODO: Fix tests
func TestBuildTree_ReturnsError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		syntaxTree       SyntaxTree
		query            string
		expectedErrorMsg string
	}{
		"example missing opening bracket": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query:            "(1+2))*3",
			expectedErrorMsg: "failed to parse query: missing opening bracket '('",
		},
		"example missing closing bracket": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
			},
			query:            "(1+(2*3)",
			expectedErrorMsg: "failed to parse query: missing closing bracket ')'",
		},
		"example parsing error typo last part": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
			},
			query:            "concat('#',name) qe '#test'",
			expectedErrorMsg: "failed to parse query: possible typo in \"( name ) qe '#test'\"",
		},
		"example parsing error typo first part": {
			syntaxTree: SyntaxTree{
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
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
			err := syntaxTree.BuildTree(query)

			// Assert
			Error(t, err)
			Equal(t, err.Error(), testData.expectedErrorMsg)
		})
	}
}

// TODO: Fix tests
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
	"1 [+]" -- "0 [1]"
	"3 [*]" -- "2 [2]"
	"3 [*]" -- "4 [3]"
	"1 [+]" -- "3 [*]"
}`,
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
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
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
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
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
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
				Lexer:       mathLexer,
				Precendence: mathPrecedence,
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
				Lexer:       odataLexer,
				Precendence: odataPrecedence,
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
