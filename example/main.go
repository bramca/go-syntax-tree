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

	mathLexer := &syntaxtree.Lexer{
		BinaryOperators:           binaryOperators,
		UnaryOperators:            unaryOperators,
		BinaryFunctions:           binaryFunctions,
		UnaryFunctions:            unaryFunctions,
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		BinaryFunctionOpSeparator: ',',
	}

	query := "-1+pow(2+3*4,pow((-1+sqrt(3))*4,3))"
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

	odataLexer := syntaxtree.Lexer{
		BinaryOperators:           binaryOperators,
		UnaryOperators:            unaryOperators,
		BinaryFunctions:           binaryFunctions,
		UnaryFunctions:            unaryFunctions,
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		StringDelimiter:           '\'',
		BinaryFunctionOpSeparator: ',',
		TokenSeparator:            ' ',
	}

	query = "name eq 'test' or contains(tolower(name), 'something (else)')"
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

	query = "name eq 'test' or anequivalent eq 'name eq contains' or contains(tolower(name), 'contains(not(an), edgecase)')"
	tokenStream = odataLexer.Tokenize(query)
	fmt.Printf("query: %s\ntokens: %+v\n", query, tokenStream)
}
