package syntaxtree

import (
	"fmt"
)

type PrattParser struct {
	Precendence map[string]int
}

func (p PrattParser) Parse(tokenStream *TokenStream, minPrecedence int, nodeId int, nodes []*Node) (*Node, int, []*Node, error) {
	lhs, newNodeId, newNodes, err := p.parsePrefix(tokenStream, nodeId, nodes)
	nodeId = newNodeId
	nodes = newNodes
	if err != nil {
		return nil, 0, nil, err
	}

	for {
		op := tokenStream.Peek()
		if op.Type == EOF {
			break
		}

		switch op.Type {
		case BinaryOperator:
			if _, ok := p.Precendence[op.Value]; !ok {
				return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("token not in precedence table: %s", op.Value)}
			}

			prec := p.Precendence[op.Value]
			if prec < minPrecedence {
				return lhs, nodeId, nodes, nil
			}

			tokenStream.Next()

			rhs, newNodeId, newNodes, err := p.Parse(tokenStream, prec, nodeId, nodes)
			if err != nil {
				return nil, 0, nil, err
			}
			nodeId = newNodeId
			nodes = newNodes

			opNode := &Node{
				Id:         nodeId,
				Value:      op.Value,
				Type:       Operator,
				RightChild: rhs,
				LeftChild:  lhs,
			}
			nodeId++
			lhs.Parent = opNode
			rhs.Parent = opNode
			if lhs.Type == LeftOperand || lhs.Type == RightOperand {
				lhs.Type = LeftOperand
			}
			if rhs.Type == LeftOperand || rhs.Type == RightOperand {
				rhs.Type = RightOperand
			}

			nodes = append(nodes, opNode)

			lhs = opNode

		case CloseDelimiter, BinaryFuncSeparator:
			return lhs, nodeId, nodes, nil

		default:
			return lhs, nodeId, nodes, nil
		}
	}

	return lhs, nodeId, nodes, nil
}

func (p PrattParser) parsePrefix(tokenStream *TokenStream, nodeId int, nodes []*Node) (*Node, int, []*Node, error) {
	token := tokenStream.Next()

	switch token.Type {
	case Operand, StringOperand:
		node := &Node{
			Id:    nodeId,
			Value: token.Value,
			Type:  LeftOperand,
		}
		nodeId++
		nodes = append(nodes, node)
		return node, nodeId, nodes, nil

	case OpenDelimiter:
		inner, newNodeId, newNodes, err := p.Parse(tokenStream, 0, nodeId, nodes)
		if err != nil {
			return nil, 0, nil, err
		}
		nodeId = newNodeId
		nodes = newNodes

		closeDelim := tokenStream.Next()
		if closeDelim.Type != CloseDelimiter {
			return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("expected ')' but got %s", closeDelim.Value)}
		}

		inner.IsGroup = true
		return inner, nodeId, nodes, nil

	case BinaryFunc:
		openDelim := tokenStream.Next()
		if openDelim.Type != OpenDelimiter {
			return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("expected '(' after binary function %s, got %s", token.Value, openDelim.Value)}
		}

		arg1, newNodeId, newNodes, err := p.Parse(tokenStream, 0, nodeId, nodes)
		if err != nil {
			return nil, 0, nil, err
		}
		nodeId = newNodeId
		nodes = newNodes

		separator := tokenStream.Next()
		if separator.Type != BinaryFuncSeparator {
			return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("expected ',' in binary function %s, got %s", token.Value, separator.Value)}
		}

		arg2, newNodeId, newNodes, err := p.Parse(tokenStream, 0, nodeId, nodes)
		if err != nil {
			return nil, 0, nil, err
		}
		nodeId = newNodeId
		nodes = newNodes

		closeDelim := tokenStream.Next()
		if closeDelim.Type != CloseDelimiter {
			return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("expected ')' after binary function %s, got %s", token.Value, closeDelim.Value)}
		}

		funcNode := &Node{
			Id:         nodeId,
			Value:      token.Value,
			Type:       Operator,
			LeftChild:  arg1,
			RightChild: arg2,
		}
		nodeId++
		arg1.Parent = funcNode
		arg2.Parent = funcNode
		if arg1.Type == LeftOperand || arg1.Type == RightOperand {
			arg1.Type = LeftOperand
		}
		if arg2.Type == LeftOperand || arg2.Type == RightOperand {
			arg2.Type = RightOperand
		}

		nodes = append(nodes, funcNode)

		return funcNode, nodeId, nodes, nil

	case UnaryFunc:
		openDelim := tokenStream.Next()
		if openDelim.Type != OpenDelimiter {
			return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("expected '(' after unary function %s, got %s", token.Value, openDelim.Value)}
		}

		arg, newNodeId, newNodes, err := p.Parse(tokenStream, 0, nodeId, nodes)
		if err != nil {
			return nil, 0, nil, err
		}
		nodeId = newNodeId
		nodes = newNodes

		closeDelim := tokenStream.Next()
		if closeDelim.Type != CloseDelimiter {
			return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("expected ')' after unary function %s, got %s", token.Value, closeDelim.Value)}
		}

		funcNode := &Node{
			Id:        nodeId,
			Value:     token.Value,
			Type:      UnaryFunction,
			LeftChild: arg,
		}
		nodeId++
		arg.Parent = funcNode
		if arg.Type != UnaryFunction {
			arg.Type = LeftOperand
		}

		nodes = append(nodes, funcNode)

		return funcNode, nodeId, nodes, nil

	case UnaryOperator:
		operand, newNodeId, newNodes, err := p.Parse(tokenStream, 100, nodeId, nodes)
		if err != nil {
			return nil, 0, nil, err
		}
		nodeId = newNodeId
		nodes = newNodes

		opNode := &Node{
			Id:        nodeId,
			Value:     token.Value,
			Type:      UnaryFunction,
			LeftChild: operand,
		}
		nodeId++
		operand.Parent = opNode
		operand.Type = LeftOperand

		nodes = append(nodes, opNode)

		return opNode, nodeId, nodes, nil

	default:
		return nil, 0, nil, &ParseError{Msg: fmt.Sprintf("unexpected token: %s (%s)", token.Value, token.Type)}
	}
}
