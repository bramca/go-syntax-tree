package syntaxtree

import (
	"errors"
	"fmt"
	"regexp"
	"slices"
	"strings"
)

type NodeType int

const (
	Unknown NodeType = iota // 0 by default
	Operator
	UnaryOperator
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
	}

	return "Unknown"
}

// SyntaxTree
// Construct a syntax tree based on a defined syntax containing of simple
// Operators, Binary Functions and Unary Functions with there mutual precedence
// The construction of the tree will also take into account grouping using brackets '()' in the precedence
type SyntaxTree struct {
	// Root node of the tree
	Root *Node

	// List of all nodes of the tree
	Nodes []*Node

	// Precedence of the operators and functions in the syntax
	// Operators with a lower index in this array have a higher precedence over operators with a lower index
	OperatorPrecedence []string

	// Define the patterns of the syntax operators
	OperatorParsers []OperatorParser

	// Define the format of the syntax binary functions
	BinaryFunctionParsers []BinaryFunctionParser

	// Define the format of the syntax unary functions
	UnaryFunctionParsers []UnaryFunctionParser

	// Define a separator that can be used to separate the operators and operands during parsing
	// This is a string that cannot exist in the query character space
	Separator string
}

type OperatorParser struct {
	OperatorString  string
	OperatorPattern *regexp.Regexp
}

type BinaryFunctionParser struct {
	FunctionName     string
	OpeningDelimiter string
	ClosingDelimiter string
	OperandSeparator byte
}

type UnaryFunctionParser struct {
	FunctionName     string
	OpeningDelimiter string
	ClosingDelimiter string
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

func (t *SyntaxTree) ConstructTree(query string) error {
	parsedQuery, err := t.ParseQuery(query)
	if err != nil {
		return err
	}

	t.Root, _ = createTree(t, parsedQuery, 0)

	return nil
}

func (t *SyntaxTree) ParseQuery(query string) (string, error) {
	query = strings.Trim(query, " ")

	// Check query for missing brackets
	delimiterCount := 0
	for i := 0; i < len(query); i++ {
		if query[i] == '(' {
			delimiterCount++
		}
		if query[i] == ')' {
			delimiterCount--
		}
	}

	if delimiterCount > 0 {
		return "", errors.New("Missing closing bracket ')'")
	}

	if delimiterCount < 0 {
		return "", errors.New("Missing opening bracket '('")
	}

	// TODO: Add more checks for parsing (e.g. operators missing an operand, operands missing an operator or function, ...)

	for _, operatorParser := range t.OperatorParsers {
		operator := operatorParser.OperatorString
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
		for firstIndex := strings.Index(query, binaryFunctionParser.FunctionName+binaryFunctionParser.OpeningDelimiter); firstIndex > 0; firstIndex = strings.Index(query, binaryFunctionParser.FunctionName+binaryFunctionParser.OpeningDelimiter) {
			delimiterCount := 0
			totalFuncString := ""
			totalFuncIndex := 0
			separatorReplaceIndex := 0
			for i := firstIndex; i < len(query); i++ {
				if query[i] == '(' {
					delimiterCount++
				}
				if query[i] == ')' {
					delimiterCount--
					if delimiterCount == 0 {
						totalFuncString += string(query[i])
						break
					}
				}
				if delimiterCount == 1 && query[i] == binaryFunctionParser.OperandSeparator {
					separatorReplaceIndex = totalFuncIndex
				}
				totalFuncString += string(query[i])
				totalFuncIndex++
			}

			newFuncString := totalFuncString[:separatorReplaceIndex] + t.Separator + binaryFunctionParser.FunctionName + t.Separator + totalFuncString[separatorReplaceIndex+1:totalFuncIndex]
			newFuncString = strings.Replace(newFuncString, binaryFunctionParser.FunctionName+binaryFunctionParser.OpeningDelimiter, "", 1)

			query = strings.Replace(query, totalFuncString, newFuncString, 1)
		}
	}

	for _, unaryFunctionParser := range t.UnaryFunctionParsers {
		for firstIndex := strings.Index(query, unaryFunctionParser.FunctionName+unaryFunctionParser.OpeningDelimiter); firstIndex > 0; firstIndex = strings.Index(query, unaryFunctionParser.FunctionName+unaryFunctionParser.OpeningDelimiter) {
			delimiterCount := 0
			totalFuncString := ""
			totalFuncIndex := 0
			for i := firstIndex; i < len(query); i++ {
				if query[i] == '(' {
					delimiterCount++
				}
				if query[i] == ')' {
					delimiterCount--
					if delimiterCount == 0 {
						totalFuncString += string(query[i])
						break
					}
				}
				totalFuncString += string(query[i])
				totalFuncIndex++
			}

			newFuncString := totalFuncString
			newFuncString = strings.Replace(newFuncString, unaryFunctionParser.FunctionName+unaryFunctionParser.OpeningDelimiter, unaryFunctionParser.FunctionName+t.Separator+unaryFunctionParser.OpeningDelimiter, 1)

			query = strings.Replace(query, totalFuncString, newFuncString, 1)
		}
	}

	query = strings.ReplaceAll(query, "(", "("+t.Separator)
	query = strings.ReplaceAll(query, ")", t.Separator+")")

	return query, nil
}

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

		previousNode = currentNode
		currentNode = &Node{
			Id:    id,
			Type:  LeftOperand,
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
			graphData += fmt.Sprintf("\t\"%d [%s]\" -- \"%d [%s]\"\n", currentNode.Parent.Id, currentNode.Parent.Value, currentNode.Id, currentNode.Value)
			currentNode = currentNode.Parent
		}
	}
	graphData += "}"

	return graphData
}
