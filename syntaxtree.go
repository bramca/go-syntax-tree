package syntaxtree

import (
	"fmt"
	"maps"
	"regexp"
	"slices"
	"strings"
)

// NodeType specifies the type of nodes in the tree
type NodeType int

const (
	Unknown NodeType = iota // 0 by default
	// both binary functions as binary operators are put in this category
	Operator
	// both unary functions as unary operators are put in this category
	UnaryOperator
	// operators have a left and right operand, unary operators only have a left operand
	LeftOperand
	RightOperand
)

func (e NodeType) String() string {
	switch e {
	case Operator:
		return "Operator"
	case UnaryOperator:
		return "UnaryOperator"
	case LeftOperand:
		return "LeftOperand"
	case RightOperand:
		return "RightOperand"
	case Unknown:
		return "Unknown"
	default:
		return "Unknown"
	}
}

// SyntaxTree
// Construct a syntax tree based on a defined syntax containing of simple
// Binary Operators, Unary Operators, Binary Functions and Unary Functions with there mutual precedence.
type SyntaxTree struct {
	// Root node of the tree
	Root *Node

	// List of all nodes of the tree
	Nodes []*Node

	// Define a Lexer that contains:
	//   - Binary / Unary operators
	//   - Binary / Unary functions
	//   - Grouping delimiters
	//   - String delimiters
	//   - Extra tokenization configuration
	Lexer *Lexer

	// Define a precedence mapping for the binary operators
	// A higher number means higher precedence
	// Equal numbers translate into left associative parsing
	Precendence map[string]int

	// WARNING: Deprecated
	// Precedence of the operators and functions in the syntax
	// Operators with a lower index in this array have a higher precedence over operators with a lower index
	OperatorPrecedence []string

	// WARNING: Deprecated
	// Define the patterns of the syntax operators
	OperatorParsers []OperatorParser

	// WARNING: Deprecated
	// Define the format of the syntax binary functions
	BinaryFunctionParsers []BinaryFunctionParser

	// WARNING: Deprecated
	// Define the format of the syntax unary functions
	UnaryFunctionParsers []UnaryFunctionParser

	// WARNING: Deprecated
	// Define a separator that can be used to separate the operators and operands during parsing
	// This is a string that cannot exist in the query character space
	Separator string
}

// WARNING: Deprecated
type OperatorParser struct {
	OperatorString  string
	OperatorPattern *regexp.Regexp
}

// WARNING: Deprecated
type BinaryFunctionParser struct {
	FunctionName     string
	OpeningDelimiter byte
	ClosingDelimiter byte
	OperandSeparator byte
}

// WARNING: Deprecated
type UnaryFunctionParser struct {
	FunctionName     string
	OpeningDelimiter byte
	ClosingDelimiter byte
}

type Node struct {
	Id         int
	Parent     *Node
	Value      string
	Type       NodeType
	LeftChild  *Node
	RightChild *Node
	IsGroup    bool
}

// BuildTree
// builds a tree based on an input query using the Pratt parser algorithm.
// It needs the following fields in the SyntaxTree to be defined:
//
//	tree := syntaxtree.SyntaxTree{
//	    Lexer:      &syntaxtree.Lexer{...},
//	    Precedence: map[string]int{...},
//	}
func (t *SyntaxTree) BuildTree(query string) error {
	if t.Lexer == nil {
		return &ParseError{
			Msg: "no lexer defined, cannot tokenize the query",
		}
	}
	tokenStream := t.Lexer.Tokenize(query)

	parser := PrattParser{
		Precedence: t.Precendence,
	}

	minPrecendence := slices.Min(slices.Collect(maps.Values(t.Precendence))) - 1

	root, nodes, err := parser.Parse(tokenStream, minPrecendence, t.Nodes)
	if err != nil {
		return err
	}

	t.Root = root
	t.Nodes = nodes

	return nil
}

// WARNING: Deprecated
// ConstructTree
// constructs a tree based on an input query using a custom parser.
// It needs the following fields in the SyntaxTree to be defined:
//
//	tree := syntaxtree.SyntaxTree{
//	    OperatorPrecedence:    []string{...},
//	    OperatorParsers:	   []OperatorParsers{...},
//	    BinaryFunctionParsers: []BinaryFunctionParsers{...},
//	    UnaryFunctionParsers:  []UnaryFunctionParsers{...},
//	    Separator:             "...",
//	}
func (t *SyntaxTree) ConstructTree(query string) error {
	parsedQuery, err := t.ParseQuery(query)
	if err != nil {
		return err
	}

	t.Root, _ = createTree(t, parsedQuery, 0)

	return nil
}

// WARNING: Deprecated
//
//nolint:gocognit,gocyclo // complex function, no way around it
func (t *SyntaxTree) ParseQuery(query string) (string, error) {
	originalQuery := query
	query = strings.Trim(query, " ")

	// Check query for missing brackets
	delimiterCount := 0
	for _, queryPart := range query {
		if queryPart == '(' {
			delimiterCount++
		}
		if queryPart == ')' {
			delimiterCount--
		}
	}

	if delimiterCount > 0 {
		return "", &ParseError{Msg: "missing closing bracket ')'"}
	}

	if delimiterCount < 0 {
		return "", &ParseError{Msg: "missing opening bracket '('"}
	}

	operatorMap := map[string]OperatorParser{}
	binaryFunctionMap := map[string]BinaryFunctionParser{}
	unaryFunctionMap := map[string]UnaryFunctionParser{}

	for _, operatorParser := range t.OperatorParsers {
		operator := operatorParser.OperatorString
		operatorMap[operator] = operatorParser
		expression := operatorParser.OperatorPattern
		query = expression.ReplaceAllStringFunc(query, func(s string) string {
			matches := expression.FindStringSubmatch(s)
			if len(matches) == 3 {
				return matches[1] + t.Separator + operator + t.Separator + matches[2]
			}

			return operator
		})
	}

	for _, binaryFunctionParser := range t.BinaryFunctionParsers {
		binaryFunctionMap[binaryFunctionParser.FunctionName] = binaryFunctionParser
		previousIndex := -1
		for nextIndex := strings.Index(query, binaryFunctionParser.FunctionName+string(binaryFunctionParser.OpeningDelimiter)); nextIndex >= 0 && nextIndex != previousIndex; nextIndex = strings.Index(query, binaryFunctionParser.FunctionName+string(binaryFunctionParser.OpeningDelimiter)) {
			delimiterCount := 0
			var totalFuncString strings.Builder
			totalFuncIndex := 0
			separatorReplaceIndex := 0
			for i := nextIndex; i < len(query); i++ {
				if query[i] == binaryFunctionParser.OpeningDelimiter {
					delimiterCount++
				}
				if query[i] == binaryFunctionParser.ClosingDelimiter {
					delimiterCount--
					if delimiterCount == 0 {
						totalFuncString.WriteByte(query[i])

						break
					}
				}
				if delimiterCount == 1 && query[i] == binaryFunctionParser.OperandSeparator {
					separatorReplaceIndex = totalFuncIndex
				}
				totalFuncString.WriteByte(query[i])
				totalFuncIndex++
			}

			if separatorReplaceIndex == 0 {
				return "", &ParseError{
					Msg: fmt.Sprintf("function '%s' is missing an operand", binaryFunctionParser.FunctionName),
				}
			}

			var buildNewFuncString strings.Builder
			buildNewFuncString.WriteString(totalFuncString.String()[:separatorReplaceIndex])
			buildNewFuncString.WriteByte(')')
			buildNewFuncString.WriteString(t.Separator)
			buildNewFuncString.WriteString(binaryFunctionParser.FunctionName)
			buildNewFuncString.WriteString(t.Separator)
			buildNewFuncString.WriteByte('(')
			buildNewFuncString.WriteString(totalFuncString.String()[separatorReplaceIndex+1 : totalFuncIndex])
			buildNewFuncString.WriteByte(')')
			newFuncString := buildNewFuncString.String()
			newFuncString = strings.Replace(newFuncString, binaryFunctionParser.FunctionName+string(binaryFunctionParser.OpeningDelimiter), "(", 1)

			query = strings.Replace(query, totalFuncString.String(), newFuncString, 1)
			previousIndex = nextIndex
		}
	}

	for _, unaryFunctionParser := range t.UnaryFunctionParsers {
		unaryFunctionMap[unaryFunctionParser.FunctionName] = unaryFunctionParser
		previousIndex := -1
		for nextIndex := strings.Index(query, unaryFunctionParser.FunctionName+string(unaryFunctionParser.OpeningDelimiter)); nextIndex >= 0 && nextIndex != previousIndex; nextIndex = strings.Index(query, unaryFunctionParser.FunctionName+string(unaryFunctionParser.OpeningDelimiter)) {
			delimiterCount := 0
			var totalFuncString strings.Builder
			totalFuncIndex := 0
			for i := nextIndex; i < len(query); i++ {
				if query[i] == unaryFunctionParser.OpeningDelimiter {
					delimiterCount++
				}
				if query[i] == unaryFunctionParser.ClosingDelimiter {
					delimiterCount--
					if delimiterCount == 0 {
						totalFuncString.WriteByte(query[i])

						break
					}
				}
				totalFuncString.WriteByte(query[i])
				totalFuncIndex++
			}

			// if the second to last character of the total function string
			// is the opening delimiter, then the function does not have
			// an operand
			if totalFuncString.String()[totalFuncIndex-1] == unaryFunctionParser.OpeningDelimiter {
				return "", &ParseError{
					Msg: fmt.Sprintf("function '%s' is missing an operand", unaryFunctionParser.FunctionName),
				}
			}

			newFuncString := totalFuncString.String()[:totalFuncIndex] + ")"
			newFuncString = strings.Replace(newFuncString, unaryFunctionParser.FunctionName+string(unaryFunctionParser.OpeningDelimiter), unaryFunctionParser.FunctionName+t.Separator+"(", 1)

			query = strings.Replace(query, totalFuncString.String(), newFuncString, 1)
			previousIndex = nextIndex
		}
	}

	query = strings.ReplaceAll(query, "(", "("+t.Separator)
	query = strings.ReplaceAll(query, ")", t.Separator+")")

	// check for possible typos, resulting in parse failure
	parsedQuerySplit := strings.Split(query, t.Separator)
	delimiterCount = 0
	lastOpeningIndex := 0
	lastClosingIndex := 0
	for index, queryPart := range parsedQuerySplit {
		if _, ok := operatorMap[queryPart]; ok {
			if index-1 < 0 || parsedQuerySplit[index-1] == "" {
				return "", &ParseError{Msg: fmt.Sprintf("operator '%s' does not have a left operand", queryPart)}
			}
			if index+1 > len(parsedQuerySplit) || parsedQuerySplit[index+1] == "" {
				return "", &ParseError{Msg: fmt.Sprintf("operator '%s' does not have a right operand", queryPart)}
			}
		}

		if queryPart == "(" {
			delimiterCount++
			lastOpeningIndex = index
		}
		if queryPart == ")" {
			delimiterCount--
			lastClosingIndex = index
		}
	}

	if delimiterCount < 0 {
		return "", &ParseError{Msg: fmt.Sprintf("possible typo in %q", strings.Join(parsedQuerySplit[lastOpeningIndex:lastClosingIndex], " "))}
	}

	if delimiterCount > 0 {
		return "", &ParseError{Msg: fmt.Sprintf("possible typo in %q", strings.Join(parsedQuerySplit[lastOpeningIndex:], " "))}
	}

	if originalQuery == query {
		return "", &ParseError{Msg: fmt.Sprintf("possible typo in %q", originalQuery)}
	}

	return query, nil
}

// WARNING: Deprecated
//
//nolint:gocognit,nestif,gocyclo,gocritic // complex function, no way around it
func createTree(t *SyntaxTree, parsedQuery string, startId int) (*Node, int) {
	var currentNode *Node
	var previousNode *Node

	parsedQuerySplit := strings.Split(parsedQuery, t.Separator)
	id := startId
	for index := 0; index < len(parsedQuerySplit); index++ {
		parsedQueryPart := parsedQuerySplit[index]
		if parsedQueryPart == ")" {
			continue
		}
		if parsedQueryPart == "(" {
			// if we find parantheses we need to create a sub syntax tree that has precedence over other parts of the tree
			closingIndex := 0
			delimiterCount := 0
			for i := index; i < len(parsedQuerySplit); i++ {
				if parsedQuerySplit[i] == "(" {
					delimiterCount++
				}
				if parsedQuerySplit[i] == ")" {
					delimiterCount--
				}
				if delimiterCount == 0 {
					closingIndex = i

					break
				}
			}
			subparsedQuery := strings.Join(parsedQuerySplit[index+1:closingIndex], t.Separator)
			subTree, newId := createTree(t, subparsedQuery, id)
			id = newId
			previousNode = currentNode
			currentNode = subTree
			currentNode.IsGroup = true
			currentNode.Parent = previousNode
			if previousNode != nil {
				if previousNode.LeftChild == nil {
					previousNode.LeftChild = currentNode
				} else {
					if currentNode.Type != Operator && currentNode.Type != UnaryOperator {
						currentNode.Type = RightOperand
					}
					previousNode.RightChild = currentNode
				}
			}
			index = closingIndex

			t.Nodes = append(t.Nodes, currentNode)

			continue
		} else if slices.Contains(t.OperatorPrecedence, parsedQueryPart) {
			previousNode = currentNode
			operatorType := Operator
			for _, unaryFunction := range t.UnaryFunctionParsers {
				if parsedQueryPart == unaryFunction.FunctionName {
					operatorType = UnaryOperator
				}
			}
			if previousNode == nil && operatorType == UnaryOperator {
				currentNode = &Node{
					Id:    id,
					Type:  operatorType,
					Value: parsedQueryPart,
				}

				t.Nodes = append(t.Nodes, currentNode)

				id++

				continue
			}
			if previousNode != nil && operatorType == UnaryOperator {
				currentNode = &Node{
					Id:     id,
					Type:   operatorType,
					Parent: previousNode,
					Value:  parsedQueryPart,
				}
				if previousNode.LeftChild == nil {
					previousNode.LeftChild = currentNode
				} else {
					if currentNode.Type != Operator && currentNode.Type != UnaryOperator {
						currentNode.Type = RightOperand
					}
					previousNode.RightChild = currentNode
				}

				t.Nodes = append(t.Nodes, currentNode)

				id++

				continue
			}
			for previousNode.Parent != nil {
				// if the previous node parent is an operator and its precedence is lower (higher index in array) then the current operator
				// we can stop the loop
				if previousNode.Parent.Type == Operator && slices.Index(t.OperatorPrecedence, previousNode.Parent.Value) > slices.Index(t.OperatorPrecedence, parsedQueryPart) && !previousNode.Parent.IsGroup {
					break
				}
				previousNode = previousNode.Parent
			}
			currentNode = &Node{
				Id:        id,
				Type:      operatorType,
				LeftChild: previousNode,
				Value:     parsedQueryPart,
			}

			if previousNode.Type != Operator && previousNode.Type != UnaryOperator {
				previousNode.Type = LeftOperand
			}
			currentNode.Parent = previousNode.Parent
			if previousNode.Parent != nil {
				if previousNode.Parent.LeftChild == nil {
					previousNode.Parent.LeftChild = currentNode
				} else {
					previousNode.Parent.RightChild = currentNode
				}
			}
			previousNode.Parent = currentNode

			t.Nodes = append(t.Nodes, currentNode)

			id++

			continue
		}

		if currentNode != nil && currentNode.Type == Operator {
			previousNode = currentNode
			currentNode = &Node{
				Id:     id,
				Type:   RightOperand,
				Parent: previousNode,
				Value:  parsedQueryPart,
			}
			previousNode.RightChild = currentNode

			t.Nodes = append(t.Nodes, currentNode)

			id++

			continue
		}

		nodeType := LeftOperand
		for _, unaryFunction := range t.UnaryFunctionParsers {
			if parsedQueryPart == unaryFunction.FunctionName {
				nodeType = UnaryOperator
			}
		}

		currentNode = &Node{
			Id:    id,
			Type:  nodeType,
			Value: parsedQueryPart,
		}

		t.Nodes = append(t.Nodes, currentNode)

		id++
	}

	for currentNode.Parent != nil {
		currentNode = currentNode.Parent
	}

	return currentNode, id
}

func (t SyntaxTree) String() string {
	if t.Root == nil {
		return ""
	}

	currentNode := t.Root
	graphData := "graph {\n"
	nodesVisited := map[int]bool{}
	for !nodesVisited[currentNode.Id] {
		if currentNode.Type == Operator || currentNode.Type == UnaryOperator {
			if currentNode.LeftChild != nil && !nodesVisited[currentNode.LeftChild.Id] {
				currentNode = currentNode.LeftChild

				continue
			}
			if currentNode.RightChild != nil && !nodesVisited[currentNode.RightChild.Id] {
				currentNode = currentNode.RightChild

				continue
			}
		}
		nodesVisited[currentNode.Id] = true
		if currentNode.Parent != nil {
			graphData = fmt.Sprintf("%s\t\"%d [%s]\" -- \"%d [%s]\"\n", graphData, currentNode.Parent.Id, currentNode.Parent.Value, currentNode.Id, currentNode.Value)
			currentNode = currentNode.Parent
		}
	}
	graphData += "}"

	return graphData
}
