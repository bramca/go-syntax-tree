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
		"+":    1,
		"-":    1,
		"*":    2,
		"/":    2,
		"pow":  3,
		"sqrt": 3,
	}

	odataPrecedence = map[string]int{
		"and":              1,
		"or":               1,
		"eq":               2,
		"ne":               2,
		"gt":               2,
		"ge":               2,
		"lt":               2,
		"le":               2,
		"length":           3,
		"indexof":          3,
		"tolower":          3,
		"toupper":          3,
		"trim":             3,
		"year":             3,
		"month":            3,
		"day":              3,
		"hour":             3,
		"minute":           3,
		"second":           3,
		"fractionalsecond": 3,
		"date":             3,
		"time":             3,
		"now":              3,
		"round":            3,
		"floor":            3,
		"ceiling":          3,
		"concat":           3,
		"contains":         3,
		"endswith":         3,
		"startswith":       3,
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
		t.Error("Expected err not to be nil but it is")
	}
}

func NoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Errorf("Expected error to be nil but it is not. err: %v", err)
	}
}
