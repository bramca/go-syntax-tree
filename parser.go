package syntaxtree

import (
	"fmt"
	"slices"
)

type PrattParser struct {
	Precedence map[string]int
}

func (p PrattParser) Parse(tokenStream *TokenStream, minPrecedence int, nodes []*Node) (*Node, []*Node, error) {
	nodeId := 0
	return p.parse(tokenStream, minPrecedence, &nodeId, nodes, 0)
}

func (p PrattParser) parse(tokenStream *TokenStream, precedence int, nodeId *int, nodes []*Node, groupDepth int) (*Node, []*Node, error) {
	lhs, newNodes, err := p.parsePrefix(tokenStream, nodeId, nodes, groupDepth)
	nodes = append(newNodes, lhs)
	if err != nil {
		return nil, nil, err
	}

	if tokenStream.Peek().Type == CloseDelimiter {
		if groupDepth == 0 {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected %q without matching %q", tokenStream.Peek().Value, "(")}
		}

		return lhs, nodes, nil
	}

	if tokenStream.Peek().Type == BinaryFuncSeparator {
		if groupDepth == 0 {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected %q outside of function call", tokenStream.Peek().Value)}
		}

		return lhs, nodes, nil
	}

	currentPrecedence, err := p.getPrecedence(tokenStream.Peek())
	if err != nil {
		return nil, nil, err
	}
	for op := tokenStream.Peek(); p.validOpType(op) && currentPrecedence >= precedence; op = tokenStream.Peek() {
		lhs, nodes, err = p.parseInfix(tokenStream, lhs, op, nodeId, nodes, currentPrecedence, groupDepth)
		if err != nil {
			return nil, nil, err
		}

		currentPrecedence, err = p.getPrecedence(tokenStream.Peek())
		if err != nil {
			return nil, nil, err
		}
	}

	return lhs, nodes, nil
}

func (p PrattParser) parsePrefix(tokenStream *TokenStream, nodeId *int, nodes []*Node, groupDepth int) (*Node, []*Node, error) {
	token := tokenStream.Next()

	switch token.Type {
	case Operand, StringOperand:
		node := &Node{
			Id:    *nodeId,
			Value: token.Value,
			Type:  LeftOperand,
		}
		(*nodeId)++
		return node, nodes, nil

	case OpenDelimiter:
		inner, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes, groupDepth+1)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		closeDelim := tokenStream.Next()
		if closeDelim.Type != CloseDelimiter {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected ')' but got %q", closeDelim.Value)}
		}

		inner.IsGroup = true
		return inner, nodes, nil

	case BinaryFunc:
		openDelim := tokenStream.Next()
		if openDelim.Type != OpenDelimiter {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected '(' after binary function %s, got %q", token.Value, openDelim.Value)}
		}

		arg1, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes, groupDepth+1)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		separator := tokenStream.Next()
		if separator.Type != BinaryFuncSeparator {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected ',' in binary function %s, got %q", token.Value, separator.Value)}
		}

		arg2, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes, groupDepth+1)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		closeDelim := tokenStream.Next()
		if closeDelim.Type != CloseDelimiter {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected ')' after binary function %s, got %q", token.Value, closeDelim.Value)}
		}

		funcNode := &Node{
			Id:         *nodeId,
			Value:      token.Value,
			Type:       Operator,
			LeftChild:  arg1,
			RightChild: arg2,
		}
		(*nodeId)++
		arg1.Parent = funcNode
		arg2.Parent = funcNode
		if arg1.Type != Operator && arg1.Type != UnaryFunction {
			arg1.Type = LeftOperand
		}
		if arg2.Type != Operator && arg2.Type != UnaryFunction {
			arg2.Type = RightOperand
		}

		return funcNode, nodes, nil

	case UnaryFunc:
		openDelim := tokenStream.Next()
		if openDelim.Type != OpenDelimiter {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected '(' after unary function %s, got %q", token.Value, openDelim.Value)}
		}

		arg, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes, groupDepth+1)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		closeDelim := tokenStream.Next()
		if closeDelim.Type != CloseDelimiter {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected ')' after unary function %s, got %q", token.Value, closeDelim.Value)}
		}

		funcNode := &Node{
			Id:        *nodeId,
			Value:     token.Value,
			Type:      UnaryFunction,
			LeftChild: arg,
		}
		(*nodeId)++
		arg.Parent = funcNode
		if arg.Type != UnaryFunction && arg.Type != Operator {
			arg.Type = LeftOperand
		}

		return funcNode, nodes, nil

	case UnaryOperator:
		operand, newNodes, err := p.parse(tokenStream, 100, nodeId, nodes, groupDepth)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		opNode := &Node{
			Id:        *nodeId,
			Value:     token.Value,
			Type:      UnaryFunction,
			LeftChild: operand,
		}
		(*nodeId)++
		operand.Parent = opNode
		operand.Type = LeftOperand

		return opNode, nodes, nil

	default:
		return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected token: %q (%s)", token.Value, token.Type)}
	}
}

func (p PrattParser) parseInfix(tokenStream *TokenStream, lhs *Node, op Token, nodeId *int, nodes []*Node, precedence int, groupDepth int) (*Node, []*Node, error) {
	switch op.Type {
	case BinaryOperator:
		tokenStream.Next()

		rhs, newNodes, err := p.parse(tokenStream, precedence+1, nodeId, nodes, groupDepth)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		opNode := &Node{
			Id:         *nodeId,
			Value:      op.Value,
			Type:       Operator,
			RightChild: rhs,
			LeftChild:  lhs,
		}
		(*nodeId)++
		lhs.Parent = opNode
		rhs.Parent = opNode
		if lhs.Type != Operator && lhs.Type != UnaryFunction {
			lhs.Type = LeftOperand
		}
		if rhs.Type != Operator && rhs.Type != UnaryFunction {
			rhs.Type = RightOperand
		}

		nodes = append(nodes, opNode)

		lhs = opNode

		return lhs, nodes, nil
	case CloseDelimiter:
		if groupDepth == 0 {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected %q without matching %q", tokenStream.Peek().Value, "(")}
		}

		return lhs, nodes, nil

	case BinaryFuncSeparator:
		if groupDepth == 0 {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected %q outside of function call", tokenStream.Peek().Value)}
		}

		return lhs, nodes, nil
	}

	return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected token %q (%s) after %q (%s)", op.Value, op.Type, lhs.Value, lhs.Type)}
}

func (p PrattParser) validOpType(op Token) bool {
	return !slices.Contains([]TokenType{EOF, CloseDelimiter, BinaryFuncSeparator}, op.Type)
}

func (p PrattParser) getPrecedence(token Token) (int, error) {
	if token.Type == BinaryOperator {
		if prec, ok := p.Precedence[token.Value]; !ok {
			return 0, &ParseError{Msg: fmt.Sprintf("token %q not in precedence table", token.Value)}
		} else {
			return prec, nil
		}
	}

	return 0, nil
}
