package main

import (
	"fmt"

	syntaxtree "github.com/bramca/go-syntax-tree"
)

func main() {
	operators := []string{
		"*",
		"/",
		"+",
		"-",
	}

	binaryFunctions := []string{
		"pow",
	}

	unaryFunctions := []string{
		"sqrt",
	}

	query := "1+pow(2+3*4,pow((1+sqrt(3))*4))"

	lexer := syntaxtree.NewLexer(operators, binaryFunctions, unaryFunctions, '(', ')', byte(0), ',', query)

	fmt.Printf("query: %s\nlexer: %+v\n", query, lexer)

	operators = []string{
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

	lexer = syntaxtree.NewLexer(operators, binaryFunctions, unaryFunctions, '(', ')', '\'', ',', query)

	fmt.Printf("query: %s\nlexer: %+v\n", query, lexer)

	query = "tolower(name) eq 'test'"

	lexer = syntaxtree.NewLexer(operators, binaryFunctions, unaryFunctions, '(', ')', '\'', ',', query)

	fmt.Printf("query: %s\nlexer: %+v\n", query, lexer)

	query = "contains(tolower(name), 'some value')"

	lexer = syntaxtree.NewLexer(operators, binaryFunctions, unaryFunctions, '(', ')', '\'', ',', query)

	fmt.Printf("query: %s\nlexer: %+v\n", query, lexer)

	query = "not(contains(tolower(name), 'some value'))"

	lexer = syntaxtree.NewLexer(operators, binaryFunctions, unaryFunctions, '(', ')', '\'', ',', query)

	fmt.Printf("query: %s\nlexer: %+v\n", query, lexer)
}
