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
