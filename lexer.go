package syntaxtree

import (
	"fmt"
	"strings"
)

type TokenType int

const (
	Atomic TokenType = iota
	String
	Operand
	BinaryFunc
	UnaryFunc
	OpenDelimiter
	CloseDelimiter
	Separator
	EOF
)

func (t TokenType) String() string {
	switch t {
	case Atomic:
		return "Atomic"
	case String:
		return "String"
	case Operand:
		return "Operand"
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

type Lexer struct {
	Tokens []Token
}

func NewLexer(operators []string, binaryFunctions []string, unaryFunctions []string, openingDelimiter byte, closingDelimiter byte, strDelimiter byte, binaryFuncOperandSeperator byte, input string) *Lexer {
	operatorIndices := map[int]string{}
	for _, op := range operators {
		inputCopy := input
		inputOffset := 0
		for index := strings.Index(input, op); index >= 0; index = strings.Index(inputCopy, op) {
			operatorIndices[index+inputOffset] = op
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	binaryFuncIndices := map[int]string{}
	for _, op := range binaryFunctions {
		inputCopy := input
		inputOffset := 0
		for index := strings.Index(input, op); index >= 0; index = strings.Index(inputCopy, op) {
			binaryFuncIndices[index+inputOffset] = op
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	unaryFuncIndices := map[int]string{}
	for _, op := range unaryFunctions {
		inputCopy := input
		inputOffset := 0
		for index := strings.Index(input, op); index >= 0; index = strings.Index(inputCopy, op) {
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
	operandType := Atomic
	for i < len(input) {
		foundType := false
		var token Token
		if op, ok := operatorIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  Operand,
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
		} else if input[i] == openingDelimiter && operandType != String {
			token = Token{
				Value: string(input[i]),
				Type:  OpenDelimiter,
			}
			i++
			foundType = true
		} else if input[i] == closingDelimiter && operandType != String {
			token = Token{
				Value: string(input[i]),
				Type:  CloseDelimiter,
			}
			i++
			foundType = true
		} else if input[i] == strDelimiter {
			if operandType == Atomic {
				operandType = String
			} else {
				operand.WriteByte(input[i])
				token = Token{
					Value: operand.String(),
					Type:  String,
				}
				foundType = true
				operandType = Atomic
				operand.Reset()
				i++
			}
		} else if input[i] == binaryFuncOperandSeperator {
			token = Token{
				Value: string(input[i]),
				Type:  Separator,
			}
			i++
			foundType = true
		} else if input[i] == ' ' && operandType != String {
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
			operand.WriteByte(input[i])
			i++
		}
	}

	return &Lexer{Tokens: tokens}
}

func (l *Lexer) Next() Token {
	token := Token{
		Type: EOF,
	}
	if len(l.Tokens) > 0 {
		token = l.Tokens[len(l.Tokens)-1]
		l.Tokens = l.Tokens[:len(l.Tokens)-1]
	}

	return token
}

func (l *Lexer) Peek() Token {
	token := Token{
		Type: EOF,
	}
	if len(l.Tokens) > 0 {
		token = l.Tokens[len(l.Tokens)-1]
	}

	return token
}
