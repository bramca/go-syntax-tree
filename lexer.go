package syntaxtree

import (
	"fmt"
	"strings"
)

type TokenType int

const (
	Operand TokenType = iota
	StringOperand
	UnaryOperator
	BinaryOperator
	BinaryFunc
	UnaryFunc
	OpenDelimiter
	CloseDelimiter
	Separator
	EOF
)

func (t TokenType) String() string {
	switch t {
	case Operand:
		return "Operand"
	case StringOperand:
		return "StringOperand"
	case BinaryOperator:
		return "BinaryOperator"
	case BinaryFunc:
		return "BinaryFunc"
	case UnaryFunc:
		return "UnaryFunc"
	case OpenDelimiter:
		return "OpenDelimiter"
	case CloseDelimiter:
		return "CloseDelimiter"
	case Separator:
		return "Separator"
	}

	return "Unknown"
}

type Token struct {
	Value string
	Type  TokenType
}

type TokenStream struct {
	Tokens []Token
}

func (t *TokenStream) Next() Token {
	token := Token{
		Type: EOF,
	}
	if len(t.Tokens) > 0 {
		token = t.Tokens[len(t.Tokens)-1]
		t.Tokens = t.Tokens[:len(t.Tokens)-1]
	}

	return token
}

func (t *TokenStream) Peek() Token {
	token := Token{
		Type: EOF,
	}
	if len(t.Tokens) > 0 {
		token = t.Tokens[len(t.Tokens)-1]
	}

	return token
}

type Lexer struct {
	BinaryOperators []string
	UnaryOperators  []string
	BinaryFunctions []string
	UnaryFunctions  []string

	OpenDelimiter             byte
	CloseDelimiter            byte
	StringDelimiter           byte
	BinaryFunctionOpSeparator byte
}

func (l *Lexer) Tokenize(expression string) *TokenStream {
	operatorIndices := map[int]string{}
	for _, op := range l.BinaryOperators {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			operatorIndices[index+inputOffset] = op
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	binaryFuncIndices := map[int]string{}
	for _, op := range l.BinaryFunctions {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			binaryFuncIndices[index+inputOffset] = op
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	unaryFuncIndices := map[int]string{}
	for _, op := range l.UnaryFunctions {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			unaryFuncIndices[index+inputOffset] = op
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	fmt.Printf("operatorIndices: %+v\n", operatorIndices)
	fmt.Printf("binaryFunctions: %+v\n", binaryFuncIndices)
	fmt.Printf("unaryFunctions: %+v\n", unaryFuncIndices)

	tokens := []Token{}

	i := 0
	var operand strings.Builder
	operandType := Operand
	for i < len(expression) {
		foundType := false
		var token Token
		if op, ok := operatorIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  BinaryOperator,
			}
			i += len(op)
			foundType = true
		} else if op, ok := binaryFuncIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  BinaryFunc,
			}
			i += len(op)
			foundType = true
		} else if op, ok := unaryFuncIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  UnaryFunc,
			}
			i += len(op)
			foundType = true
		} else if expression[i] == l.OpenDelimiter && operandType != StringOperand {
			token = Token{
				Value: string(expression[i]),
				Type:  OpenDelimiter,
			}
			i++
			foundType = true
		} else if expression[i] == l.CloseDelimiter && operandType != StringOperand {
			token = Token{
				Value: string(expression[i]),
				Type:  CloseDelimiter,
			}
			i++
			foundType = true
		} else if expression[i] == l.StringDelimiter {
			if operandType == Operand {
				operandType = StringOperand
			} else {
				operand.WriteByte(expression[i])
				token = Token{
					Value: operand.String(),
					Type:  StringOperand,
				}
				foundType = true
				operandType = Operand
				operand.Reset()
				i++
			}
		} else if expression[i] == l.BinaryFunctionOpSeparator {
			token = Token{
				Value: string(expression[i]),
				Type:  Separator,
			}
			i++
			foundType = true
		} else if expression[i] == ' ' && operandType != StringOperand {
			i++
			continue
		}

		if foundType && operand.Len() > 0 {
			tokens = append(tokens, Token{
				Value: operand.String(),
				Type:  operandType,
			})
			tokens = append(tokens, token)
			operand.Reset()
		} else if foundType {
			tokens = append(tokens, token)
		} else {
			operand.WriteByte(expression[i])
			i++
		}
	}

	return &TokenStream{Tokens: tokens}
}
