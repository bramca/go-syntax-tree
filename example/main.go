package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"

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

	odataLexer := &syntaxtree.Lexer{
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

	mathTree := syntaxtree.SyntaxTree{
		Lexer: mathLexer,
		Precendence: map[string]int{
			"+":    1,
			"-":    1,
			"*":    2,
			"/":    2,
			"pow":  3,
			"sqrt": 3,
		},
	}

	odataTree := syntaxtree.SyntaxTree{
		Lexer: odataLexer,
		Precendence: map[string]int{
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
		},
	}

	testCasesMath := []struct {
		name  string
		query string
	}{
		// {"Basic binary operators", "1+2*3"},
		// {"Grouping", "(1+2)*3"},
		// {"Unary functions", "sqrt(4)+sqrt(9)"},
		// {"Binary functions", "pow(2,3)+sqrt(4)"},
		// {"Nested functions with operators", "pow(2,3+1)*sqrt(4)"},
		// {"Complex expression with unary operators and nested functions", "-1+pow(2+3*4,pow((-1+sqrt(3))*4,3))"},
	}

	testCasesOdata := []struct {
		name  string
		query string
	}{
		{"odata simple example", "toupper(tolower(name)) eq 'JOHN'"},
		{"odata simple example multibyte", "contains(tolower(name), 'café')"},
		{"odata complex example edge cases", "name eq 'test' or anequivalent eq 'name eq contains' or contains(tolower(name), 'contains(not(an), edgecase)')"},
		{"odata complex example", "name eq 'John' and (concat(lastname,concat(' ', name)) eq 'Smith John' or contains(concat(name,lastname),'Smith') or length(concat(name,lastname)) eq 10)"},
	}

	for i, tc := range testCasesMath {
		fmt.Printf("=== Test %d: %s ===\n", i+1, tc.name)
		fmt.Printf("Query: %s\n", tc.query)
		err := mathTree.BuildTree(tc.query)
		if err != nil {
			fmt.Printf("err: %s\n", err)
			os.Exit(2)
		}

		fmt.Println("Nodes list:")
		for _, node := range mathTree.Nodes {
			fmt.Printf("%d-[%s]-[%s] ", node.Id, node.Value, node.Type)
		}
		fmt.Println()
		fmt.Printf("Tree (dotfile format):\n%s\n\n", mathTree)
		err = os.WriteFile("tree.dot", []byte(mathTree.String()), os.ModePerm)
		if err != nil {
			fmt.Printf("err: %s\n", err)
			os.Exit(2)
		}
		cmd := exec.Command("dot", "-Tpng", "tree.dot", "-o", "tree.png")
		err = cmd.Run()
		if err != nil {
			fmt.Printf("dot command err: %s\n", err)
			os.Exit(2)
		}

		imgCmd := exec.Command("wezterm", "imgcat", "tree.png")
		stdout, _ := imgCmd.StdoutPipe()
		err = imgCmd.Start()
		if err != nil {
			fmt.Printf("error starting wezterm command: %s\n", err)
		}

		scanner := bufio.NewScanner(stdout)
		scanner.Split(bufio.ScanWords)
		for scanner.Scan() {
			m := scanner.Text()
			fmt.Println(m)
		}
		err = imgCmd.Wait()
		if err != nil {
			fmt.Printf("err while waiting: %s\n", err)
		}
	}

	for i, tc := range testCasesOdata {
		fmt.Printf("=== Test %d: %s ===\n", i+1, tc.name)
		fmt.Printf("Query: %s\n", tc.query)
		err := odataTree.BuildTree(tc.query)
		if err != nil {
			fmt.Printf("err: %s\n", err)
			os.Exit(2)
		}

		fmt.Println("Nodes list:")
		for _, node := range odataTree.Nodes {
			fmt.Printf("%d-[%s]-[%s] ", node.Id, node.Value, node.Type)
		}
		fmt.Println()
		fmt.Printf("Tree (dotfile format):\n%s\n\n", odataTree)
		err = os.WriteFile("tree.dot", []byte(odataTree.String()), os.ModePerm)
		if err != nil {
			fmt.Printf("err: %s\n", err)
			os.Exit(2)
		}
		cmd := exec.Command("dot", "-Tpng", "tree.dot", "-o", "tree.png")
		err = cmd.Run()
		if err != nil {
			fmt.Printf("dot command err: %s\n", err)
			os.Exit(2)
		}

		imgCmd := exec.Command("wezterm", "imgcat", "tree.png")
		stdout, _ := imgCmd.StdoutPipe()
		err = imgCmd.Start()
		if err != nil {
			fmt.Printf("error starting wezterm command: %s\n", err)
		}
		scanner := bufio.NewScanner(stdout)
		scanner.Split(bufio.ScanWords)
		for scanner.Scan() {
			m := scanner.Text()
			fmt.Println(m)
		}
		err = imgCmd.Wait()
		if err != nil {
			fmt.Printf("err while waiting: %s\n", err)
		}
	}
}
