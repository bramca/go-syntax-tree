package main

import (
	"fmt"

	syntaxtree "github.com/bramca/go-syntax-tree"
)

func main() {
	binaryOperators := []string{
		"*",
		"/",
		"+",
		"-",
	}

	unaryOperators := []string{
		"-",
	}

	binaryFunctions := []string{
		"pow",
	}

	unaryFunctions := []string{
		"sqrt",
	}

	query := "1+pow(2+3*4,pow((1+sqrt(3))*4))"

	mathLexer := &syntaxtree.Lexer{
		BinaryOperators: binaryOperators,
		UnaryOperators:  unaryOperators,
		BinaryFunctions: binaryFunctions,
		UnaryFunctions:  unaryFunctions,
		OpenDelimiter:   '(',
		CloseDelimiter:  ')',
	}

	tokenStream := mathLexer.Tokenize(query)

	fmt.Printf("query: %s\ntokens: %+v\n", query, tokenStream)

	binaryOperators = []string{
		"eq",
		"ne",
		"and",
		"or",
	}

	binaryFunctions = []string{
		"contains",
	}

	unaryFunctions = []string{
		"tolower",
		"not",
	}

	query = "name eq 'test' or contains(tolower(name), 'something (else)')"

	odataLexer := syntaxtree.Lexer{
		BinaryOperators:           binaryOperators,
		UnaryOperators:            unaryOperators,
		BinaryFunctions:           binaryFunctions,
		UnaryFunctions:            unaryFunctions,
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		StringDelimiter:           '\'',
		BinaryFunctionOpSeparator: ',',
	}

	tokenStream = odataLexer.Tokenize(query)

	fmt.Printf("query: %s\ntokens: %+v\n", query, tokenStream)

	query = "tolower(name) eq 'test'"

	tokenStream = odataLexer.Tokenize(query)

	fmt.Printf("query: %s\ntokens: %+v\n", query, tokenStream)

	query = "contains(tolower(name), 'some value')"

	tokenStream = odataLexer.Tokenize(query)

	fmt.Printf("query: %s\ntokens: %+v\n", query, tokenStream)

	query = "not(contains(tolower(name), 'some value'))"

	tokenStream = odataLexer.Tokenize(query)

	fmt.Printf("query: %s\ntokens: %+v\n", query, tokenStream)
}
