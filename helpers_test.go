package syntaxtree

import (
	"fmt"
	"regexp"
	"testing"
)

var (
	mathLexer = &Lexer{
		BinaryOperators: []string{
			"*",
			"/",
			"+",
			"-",
		},
		UnaryOperators: []string{
			"-",
		},
		BinaryFunctions: []string{
			"pow",
		},
		UnaryFunctions: []string{
			"sqrt",
		},
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		BinaryFunctionOpSeparator: ',',
	}

	odataLexer = &Lexer{
		BinaryOperators: []string{
			"eq",
			"ne",
			"gt",
			"ge",
			"lt",
			"le",
			"and",
			"or",
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
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		BinaryFunctionOpSeparator: ',',
		StringDelimiter:           '\'',
		TokenSeparator:            ' ',
	}

	mathPrecedence = map[string]int{
		"+": 1,
		"-": 1,
		"*": 2,
		"/": 2,
	}

	odataPrecedence = map[string]int{
		"or":  1,
		"and": 2,
		"eq":  3,
		"ne":  3,
		"gt":  3,
		"ge":  3,
		"lt":  3,
		"le":  3,
	}

	// WARNING: Deprecated
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
		t.Fatal("Expected err not to be nil but it is")
	}
}

func NoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Fatalf("Expected error to be nil but it is not. err: %v", err)
	}
}

// WARNING: Deprecated
type Example struct {
	OperatorPrecedence []string
	OperatorParsers    []OperatorParser
	BinaryFunctions    []string
	UnaryFunctions     []string
}

// WARNING: Deprecated
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

// WARNING: Deprecated
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
