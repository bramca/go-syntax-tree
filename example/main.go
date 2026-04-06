package main

import (
	"fmt"
	"os"
	"regexp"

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

	query := "1+2*3"
	tree := syntaxtree.SyntaxTree{
		Lexer: mathLexer,
		Precendence: map[string]int{
			"+": 1,
			"-": 1,
			"*": 2,
			"/": 2,
		},
	}

	// new tree
	err := tree.BuildTree(query)
	if err != nil {
		fmt.Printf("err: %s\n", err)
		os.Exit(2)
	}

	fmt.Printf("new tree:\n%s\n", tree)
	fmt.Printf("new tree nodes:\n")
	for _, node := range tree.Nodes {
		fmt.Printf("\t- %+v\n", node)
	}

	operatorsPrecedence := []string{
		"pow",
		"sqrt",
		"/",
		"*",
		"+",
		"-",
	}

	// old tree
	binaryFunctionParsers := make([]syntaxtree.BinaryFunctionParser, len(binaryFunctions))
	for i, binaryFunction := range binaryFunctions {
		binaryFunctionParsers[i] = syntaxtree.BinaryFunctionParser{
			FunctionName:     binaryFunction,
			OpeningDelimiter: '(',
			ClosingDelimiter: ')',
			OperandSeparator: ',',
		}
	}

	unaryFunctionParsers := make([]syntaxtree.UnaryFunctionParser, len(unaryFunctions))
	for i, unaryFunction := range unaryFunctions {
		unaryFunctionParsers[i] = syntaxtree.UnaryFunctionParser{
			FunctionName:     unaryFunction,
			OpeningDelimiter: '(',
			ClosingDelimiter: ')',
		}
	}

	operatorParsers := make([]syntaxtree.OperatorParser, len(binaryOperators))
	for i, operator := range binaryOperators {
		operatorParsers[i] = syntaxtree.OperatorParser{
			OperatorString:  operator,
			OperatorPattern: regexp.MustCompile(fmt.Sprintf(`([\d\(\)]*)\%s([\d\(\)]*|pow|sqrt)`, operator)),
		}
	}

	oldTree := syntaxtree.SyntaxTree{
		OperatorPrecedence:    operatorsPrecedence,
		OperatorParsers:       operatorParsers,
		BinaryFunctionParsers: binaryFunctionParsers,
		UnaryFunctionParsers:  unaryFunctionParsers,
		Separator:             ";",
	}

	err = oldTree.ConstructTree(query)
	if err != nil {
		fmt.Printf("err: %s\n", err)
		os.Exit(2)
	}

	fmt.Printf("old tree: \n%s\n", oldTree)
	fmt.Printf("old tree nodes:\n")
	for _, node := range oldTree.Nodes {
		fmt.Printf("\t- %+v\n", node)
	}
	/*
		query = "-1+pow(2+3*4,pow((-1+sqrt(3))*4,3))"
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
	*/
}
