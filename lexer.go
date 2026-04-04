package syntaxtree

import (
	"fmt"
	"strings"
)

type TokenType int

const (
	At TokenType = iota
	Str
	Op
	BinFunc
	UnFunc
	OpenDelim
	CloseDelim
	Sep
	EOF
)

func (t TokenType) String() string {
	switch t {
	case At:
		return "At"
	case Str:
		return "Str"
	case Op:
		return "Op"
	case BinFunc:
		return "BinFunc"
	case UnFunc:
		return "UnFunc"
	case OpenDelim:
		return "OpenDelim"
	case CloseDelim:
		return "CloseDelim"
	case Sep:
		return "Sep"
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
	operandType := At
	for i < len(input) {
		foundType := false
		var token Token
		if op, ok := operatorIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  Op,
			}
			i += len(op)
			foundType = true
		} else if op, ok := binaryFuncIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  BinFunc,
			}
			i += len(op)
			foundType = true
		} else if op, ok := unaryFuncIndices[i]; ok {
			token = Token{
				Value: op,
				Type:  UnFunc,
			}
			i += len(op)
			foundType = true
		} else if input[i] == openingDelimiter && operandType != Str {
			token = Token{
				Value: string(input[i]),
				Type:  OpenDelim,
			}
			i++
			foundType = true
		} else if input[i] == closingDelimiter && operandType != Str {
			token = Token{
				Value: string(input[i]),
				Type:  CloseDelim,
			}
			i++
			foundType = true
		} else if input[i] == strDelimiter {
			if operandType == At {
				operandType = Str
			} else {
				operand.WriteByte(input[i])
				token = Token{
					Value: operand.String(),
					Type:  Str,
				}
				foundType = true
				operandType = At
				operand.Reset()
				i++
			}
		} else if input[i] == binaryFuncOperandSeperator {
			token = Token{
				Value: string(input[i]),
				Type:  Sep,
			}
			i++
			foundType = true
		} else if input[i] == ' ' && operandType != Str {
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
