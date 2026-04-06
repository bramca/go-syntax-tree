package syntaxtree

import (
	"fmt"
)

type PrattParser struct {
	Precendence map[string]int
}

func (p PrattParser) Parse(tokenStream *TokenStream, minPrecedence int, nodeId int, nodes []*Node) (*Node, []*Node, error) {
	fmt.Printf("tokenStream: %+v\n", tokenStream)
	lhsToken := tokenStream.Next()
	lhs := &Node{
		Id:    nodeId,
		Value: lhsToken.Value,
		Type:  LeftOperand,
	}
	if lhsToken.Type != Operand {
		return nil, nil, &ParseError{Msg: fmt.Sprintf("bad token: %s", lhsToken.Value)}
	}

	for {
		op := tokenStream.Peek()
		if op.Type == EOF {
			break
		}

		if op.Type != BinaryOperator {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("bad token: %s", lhs.Value)}
		}

		if _, ok := p.Precendence[op.Value]; !ok {
			return nil, nil, &ParseError{Msg: fmt.Sprintf("token not in precendence table: %s", op.Value)}
		}

		prec := p.Precendence[op.Value]

		if prec < minPrecedence {
			break
		}

		tokenStream.Next()

		nodeId++
		rhs, _, err := p.Parse(tokenStream, prec, nodeId, nodes)
		if err != nil {
			return nil, nil, err
		}

		nodeId++
		opNode := &Node{
			Id:         nodeId,
			Value:      op.Value,
			Type:       Operator,
			RightChild: rhs,
			LeftChild:  lhs,
		}
		lhs.Parent = opNode
		rhs.Parent = opNode
		rhs.Type = RightOperand

		nodes = append(nodes, lhs)
		nodes = append(nodes, rhs)

		lhs = opNode
	}

	nodes = append(nodes, lhs)

	return lhs, nodes, nil
}
