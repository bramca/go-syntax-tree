package main

import (
	"fmt"
	"os"

	syntaxtree "github.com/bramca/go-syntax-tree"
)

func main() {
	mathLexer := &syntaxtree.Lexer{
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

	mathTree := syntaxtree.SyntaxTree{
		Lexer: mathLexer,
		Precendence: map[string]int{
			"+": 1,
			"-": 1,
			"*": 2,
			"/": 2,
		},
	}

	err := mathTree.BuildTree("-1+pow(2+3*4,pow((-1+sqrt(3))*4,3))")
	if err != nil {
		fmt.Printf("err: %s\n", err)
		os.Exit(2)
	}

	sqlLexer := &syntaxtree.Lexer{
		BinaryOperators: []string{
			"AND",
			"OR",
			"=",
			"!=",
			"<",
			"<=",
			">",
			">=",
		},
		OpenDelimiter:   '(',
		CloseDelimiter:  ')',
		StringDelimiter: '\'',
		TokenSeparator:  ' ',
	}

	sqlTree := syntaxtree.SyntaxTree{
		Lexer: sqlLexer,
		Precendence: map[string]int{
			"OR":  1,
			"AND": 2,
			"=":   3,
			"!=":  3,
			"<":   3,
			"<=":  3,
			">":   3,
			">=":  3,
		},
	}

	err = sqlTree.BuildTree("name = 'ab12' OR price <= 10 AND price >= 5")
	if err != nil {
		fmt.Printf("err: %s\n", err)
		os.Exit(2)
	}

	fmt.Printf("Math Tree (dotfile format):\n%s\n", mathTree)
	fmt.Printf("SQL Tree (dotfile format):\n%s\n", sqlTree)
}
