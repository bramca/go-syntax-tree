package syntaxtree

import "testing"

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
		"and": 1,
		"or":  1,
		"eq":  2,
		"ne":  2,
		"gt":  2,
		"ge":  2,
		"lt":  2,
		"le":  2,
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
