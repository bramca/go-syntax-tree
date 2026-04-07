package syntaxtree

import (
	"fmt"
)

type PrattParser struct {
	Precendence map[string]int
}

func (p PrattParser) Parse(tokenStream *TokenStream, minPrecedence int, nodes []*Node) (*Node, []*Node, error) {
	nodeId := 0
	return p.parse(tokenStream, minPrecedence, &nodeId, nodes)
}

func (p PrattParser) parse(tokenStream *TokenStream, minPrecedence int, nodeId *int, nodes []*Node) (*Node, []*Node, error) {
	lhs, newNodes, err := p.parsePrefix(tokenStream, nodeId, nodes)
	nodes = append(newNodes, lhs)
	if err != nil {
		return nil, nil, err
	}

	for {
		op := tokenStream.Peek()
		if op.Type == EOF {
			break
		}

		switch op.Type {
		case BinaryOperator:
			if _, ok := p.Precendence[op.Value]; !ok {
				return nil, nil, &ParseError{Msg: fmt.Sprintf("token %q not in precedence table", op.Value)}
			}

			prec := p.Precendence[op.Value]
			if prec < minPrecedence {
				return lhs, nodes, nil
			}

			tokenStream.Next()

			rhs, newNodes, err := p.parse(tokenStream, prec+1, nodeId, nodes)
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

		case CloseDelimiter, BinaryFuncSeparator:
			return lhs, nodes, nil

		default:
			return nil, nil, &ParseError{Msg: fmt.Sprintf("unexpected token %q (%s) after %q (%s)", op.Value, op.Type, lhs.Value, lhs.Type)}
		}
	}

	return lhs, nodes, nil
}

func (p PrattParser) parsePrefix(tokenStream *TokenStream, nodeId *int, nodes []*Node) (*Node, []*Node, error) {
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
		inner, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes)
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

		arg1, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes)
		if err != nil {
			return nil, nil, err
		}
		nodes = newNodes

		separator := tokenStream.Next()
		if separator.Type != BinaryFuncSeparator {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("expected ',' in binary function %s, got %q", token.Value, separator.Value)}
		}

		arg2, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes)
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

		arg, newNodes, err := p.parse(tokenStream, 0, nodeId, nodes)
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
		operand, newNodes, err := p.parse(tokenStream, 100, nodeId, nodes)
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
