package syntaxtree

import (
	"fmt"
	"regexp"
	"testing"

	"github.com/bramca/go-syntax-tree/assert"
)

type Example struct {
	OperatorPrecedence []string
	OperatorParsers []OperatorParser
	BinaryFunctions []string
	UnaryFunctions []string
}

func (e Example) GetBinaryFunctionOperators(openingDelimiter string, closingDelimiter string, operatndSeparator byte) []BinaryFunctionParser {
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

func (e Example) GetUnaryFunctionOperators(openingDelimiter string, closingDelimiter string) []UnaryFunctionParser {
	unaryFunctionParsers :=  make([]UnaryFunctionParser, len(e.UnaryFunctions))
	for i, unaryFunction := range e.UnaryFunctions {
		unaryFunctionParsers[i] = UnaryFunctionParser{
			FunctionName:     unaryFunction,
			OpeningDelimiter: openingDelimiter,
			ClosingDelimiter: closingDelimiter,
		}
	}

	return unaryFunctionParsers
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
		OperatorParsers:    []OperatorParser{
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
		BinaryFunctions:    []string{
			"pow",
		},
		UnaryFunctions:     []string{
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
		OperatorParsers:    []OperatorParser{
			{
				OperatorString:  "eq",
				OperatorPattern: regexp.MustCompile(`(.*?) eq (.*?)`),
			},
			{
				OperatorString: "ne",
				OperatorPattern: regexp.MustCompile(`(.*?) ne (.*?)`),
			},
			{
				OperatorString: "gt",
				OperatorPattern: regexp.MustCompile(`(.*?) gt (.*?)`),
			},
			{
				OperatorString: "ge",
				OperatorPattern: regexp.MustCompile(`(.*?) ge (.*?)`),
			},
			{
				OperatorString: "lt",
				OperatorPattern: regexp.MustCompile(`(.*?) lt (.*?)`),
			},
			{
				OperatorString: "le",
				OperatorPattern: regexp.MustCompile(`(.*?) le (.*?)`),
			},
			{
				OperatorString: "and",
				OperatorPattern: regexp.MustCompile(`(.*?) and (.*?)`),
			},
			{
				OperatorString: "or",
				OperatorPattern: regexp.MustCompile(`(.*?) or (.*?)`),
			},

		},
		BinaryFunctions:    []string{
			"concat",
			"contains",
			"endswith",
			"startswith",
		},
		UnaryFunctions:     []string{
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

func TestParseQuery_ReturnsError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct{
		syntaxTree SyntaxTree
		query string
		expectedErrorMsg string
	}{
		"missing closing bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(",
			expectedErrorMsg: "Missing closing bracket ')'",
		},
		"missing opening bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "())",
			expectedErrorMsg: "Missing opening bracket '('",
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
			assert.Equal(t, parsedQuery, "")
			assert.Error(t, err)
			assert.Equal(t, err.Error(), testData.expectedErrorMsg)
		})
	}
}

func TestParseQuery_ReturnsCorrectQuery(t *testing.T) {
	t.Parallel()
	tests := map[string]struct{
		syntaxTree SyntaxTree
		query string
		expectedParsedQuery string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "1+2*3",
			expectedParsedQuery: "1;+;2;*;3",
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*3",
			expectedParsedQuery: "(;1;+;2;);*;3",
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(3)",
			expectedParsedQuery: "(;1;+;2;);*;sqrt;(;3;)",
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(pow(2,pow(3,3)))",
			expectedParsedQuery: "(;1;+;2;);*;sqrt;(;2;pow;3;pow;3;)",
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3,pow(3,2)))",
			expectedParsedQuery: "1;-;sqrt;(;2;pow;3;+;1;);*;2;/;(;sqrt;(;1;+;1;);*;3;pow;3;pow;2;)",
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
			expectedParsedQuery: "name;eq;'John';and;(;lastname;concat;' ';concat; name;eq;'Smith John';or;name;concat;lastname;contains;'Smith';or;length;(;name;concat;lastname;);eq;10;)",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			parsedQuery, err := syntaxTree.ParseQuery(query)

			// Assert
			assert.NoError(t, err)
			assert.Equal(t, parsedQuery, testData.expectedParsedQuery)
		})
	}
}

func TestConstructTree_ReturnsError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct{
		syntaxTree SyntaxTree
		query string
		expectedErrorMsg string
	}{
		"example missing opening bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2))*3",
			expectedErrorMsg: "Missing opening bracket '('",
		},
		"example missing closing bracket": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+(2*3)",
			expectedErrorMsg: "Missing closing bracket ')'",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.ConstructTree(query)

			// Assert
			assert.Error(t, err)
			assert.Equal(t, err.Error(), testData.expectedErrorMsg)
		})
	}
}

func TestConstructTree_ReturnsNoError(t *testing.T) {
	t.Parallel()
	tests := map[string]struct{
		syntaxTree SyntaxTree
		query string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "1+2*3",
		},
		"math simple example grouping": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*3",
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(3)",
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(pow(2,pow(3,3)))",
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3,pow(3,2)))",
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.ConstructTree(query)

			// Assert
			assert.NoError(t, err)
		})
	}
}

func TestConstructTree_CreatesCorrectGraph(t *testing.T) {
	t.Parallel()
	tests := map[string]struct{
		syntaxTree SyntaxTree
		query string
		expectedGraph string
	}{
		"math simple example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
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
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*3",
			expectedGraph: `graph {
	"1 [(+)]" -- "0 [1]"
	"1 [(+)]" -- "2 [2]"
	"3 [*]" -- "1 [(+)]"
	"3 [*]" -- "4 [3]"
}`,
		},
		"math simple example unary function": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(3)",
			expectedGraph: `graph {
	"1 [(+)]" -- "0 [1]"
	"1 [(+)]" -- "2 [2]"
	"3 [*]" -- "1 [(+)]"
	"4 [sqrt]" -- "5 [(3)]"
	"3 [*]" -- "4 [sqrt]"
}`,
		},
		"math simple example function recursion": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "(1+2)*sqrt(pow(2,pow(3,sqrt(3))))",
			expectedGraph: `graph {
	"1 [(+)]" -- "0 [1]"
	"1 [(+)]" -- "2 [2]"
	"3 [*]" -- "1 [(+)]"
	"6 [pow]" -- "5 [2]"
	"6 [pow]" -- "7 [3]"
	"8 [(pow)]" -- "6 [pow]"
	"9 [sqrt]" -- "10 [(3)]"
	"8 [(pow)]" -- "9 [sqrt]"
	"4 [sqrt]" -- "8 [(pow)]"
	"3 [*]" -- "4 [sqrt]"
}`,
		},
		"math complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleMath.OperatorPrecedence,
				OperatorParsers:       exampleMath.OperatorParsers,
				BinaryFunctionParsers: exampleMath.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleMath.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "1-sqrt(pow(2,3)+1)*2/(sqrt(1+1)*pow(3,pow(3,2)))",
			expectedGraph: `graph {
	"1 [-]" -- "0 [1]"
	"4 [pow]" -- "3 [2]"
	"4 [pow]" -- "5 [3]"
	"6 [(+)]" -- "4 [pow]"
	"6 [(+)]" -- "7 [1]"
	"2 [sqrt]" -- "6 [(+)]"
	"8 [*]" -- "2 [sqrt]"
	"10 [/]" -- "9 [2]"
	"13 [(+)]" -- "12 [1]"
	"13 [(+)]" -- "14 [1]"
	"11 [sqrt]" -- "13 [(+)]"
	"15 [(*)]" -- "11 [sqrt]"
	"17 [pow]" -- "16 [3]"
	"17 [pow]" -- "18 [3]"
	"19 [pow]" -- "17 [pow]"
	"19 [pow]" -- "20 [2]"
	"15 [(*)]" -- "19 [pow]"
	"10 [/]" -- "15 [(*)]"
	"8 [*]" -- "10 [/]"
	"1 [-]" -- "8 [*]"
}`,
		},
		"odata complex example": {
			syntaxTree: SyntaxTree{
				OperatorPrecedence:    exampleOdata.OperatorPrecedence,
				OperatorParsers:       exampleOdata.OperatorParsers,
				BinaryFunctionParsers: exampleOdata.GetBinaryFunctionOperators("(", ")", ','),
				UnaryFunctionParsers:  exampleOdata.GetUnaryFunctionOperators("(", ")"),
				Separator:             ";",
			},
			query: "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)",
			expectedGraph: `graph {
	"1 [eq]" -- "0 [name]"
	"1 [eq]" -- "2 ['John']"
	"3 [and]" -- "1 [eq]"
	"5 [concat]" -- "4 [lastname]"
	"5 [concat]" -- "6 [' ']"
	"7 [concat]" -- "5 [concat]"
	"7 [concat]" -- "8 [ name]"
	"9 [eq]" -- "7 [concat]"
	"9 [eq]" -- "10 ['Smith John']"
	"11 [or]" -- "9 [eq]"
	"13 [concat]" -- "12 [name]"
	"13 [concat]" -- "14 [lastname]"
	"15 [contains]" -- "13 [concat]"
	"15 [contains]" -- "16 ['Smith']"
	"11 [or]" -- "15 [contains]"
	"17 [(or)]" -- "11 [or]"
	"20 [(concat)]" -- "19 [name]"
	"20 [(concat)]" -- "21 [lastname]"
	"18 [length]" -- "20 [(concat)]"
	"22 [eq]" -- "18 [length]"
	"22 [eq]" -- "23 [10]"
	"17 [(or)]" -- "22 [eq]"
	"3 [and]" -- "17 [(or)]"
}`,
		},
	}

	for name, testData := range tests {
		testData := testData
		t.Run(name, func(t *testing.T) {
			// Arrange
			syntaxTree := testData.syntaxTree
			query := testData.query

			// Act
			err := syntaxTree.ConstructTree(query)

			// Assert
			assert.NoError(t, err)
			assert.Equal(t, fmt.Sprintf("%s", syntaxTree), testData.expectedGraph)
		})
	}
}
