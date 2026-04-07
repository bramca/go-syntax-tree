package syntaxtree

import (
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
	BinaryFuncSeparator
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
	case UnaryOperator:
		return "UnaryOperator"
	case BinaryFunc:
		return "BinaryFunc"
	case UnaryFunc:
		return "UnaryFunc"
	case OpenDelimiter:
		return "OpenDelimiter"
	case CloseDelimiter:
		return "CloseDelimiter"
	case BinaryFuncSeparator:
		return "BinaryFuncSeparator"
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
		token = t.Tokens[0]
		t.Tokens = t.Tokens[1:]
	}

	return token
}

func (t *TokenStream) Peek() Token {
	token := Token{
		Type: EOF,
	}
	if len(t.Tokens) > 0 {
		token = t.Tokens[0]
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
	TokenSeparator            byte
}

func (l *Lexer) Tokenize(expression string) *TokenStream {
	binaryOpIndices := map[int]string{}
	binaryOpValidatePrefix := func(prefix byte) bool {
		return (l.TokenSeparator == byte(0) || prefix == l.TokenSeparator) &&
			(l.OpenDelimiter == byte(0) || prefix != l.OpenDelimiter)
	}
	binaryOpValidateSuffix := func(suffix byte) bool {
		return (l.TokenSeparator == byte(0) || suffix == l.TokenSeparator) &&
			(l.CloseDelimiter == byte(0) || suffix != l.CloseDelimiter)
	}
	for _, op := range l.BinaryOperators {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			valid := index-1 > -1 && index+len(op) < len(inputCopy) && binaryOpValidatePrefix(inputCopy[index-1]) && binaryOpValidateSuffix(inputCopy[index+len(op)])

			if valid {
				binaryOpIndices[index+inputOffset] = op
			}
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	binaryFuncIndices := map[int]string{}
	binaryFuncValidatePrefix := func(prefix byte) bool {
		return (l.OpenDelimiter == byte(0) || prefix == l.OpenDelimiter) ||
			(l.TokenSeparator == byte(0) || prefix == l.TokenSeparator) ||
			(l.BinaryFunctionOpSeparator == byte(0) || prefix == l.BinaryFunctionOpSeparator)
	}

	binaryFuncValidateSuffix := func(suffix byte) bool {
		return l.OpenDelimiter == byte(0) || suffix == l.OpenDelimiter
	}
	for _, op := range l.BinaryFunctions {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			valid := (index == 0 || binaryFuncValidatePrefix(inputCopy[index-1])) &&
				index+len(op) < len(inputCopy) &&
				binaryFuncValidateSuffix(inputCopy[index+len(op)])
			if valid {
				binaryFuncIndices[index+inputOffset] = op
			}
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	unaryFuncIndices := map[int]string{}
	unaryFuncValidatePrefix := func(prefix byte) bool {
		return (l.OpenDelimiter == byte(0) || prefix == l.OpenDelimiter) ||
			(l.TokenSeparator == byte(0) || prefix == l.TokenSeparator)
	}
	unaryFunxValidateSuffix := func(suffix byte) bool {
		return l.OpenDelimiter == byte(0) || suffix == l.OpenDelimiter
	}
	for _, op := range l.UnaryFunctions {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			valid := (index == 0 || unaryFuncValidatePrefix(inputCopy[index-1])) && index+len(op) < len(inputCopy) && unaryFunxValidateSuffix(inputCopy[index+len(op)])
			if valid {
				unaryFuncIndices[index+inputOffset] = op
			}
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	unaryOpIndices := map[int]string{}
	unaryOpValidatePrefix := func(prefix byte) bool {
		return (l.OpenDelimiter == byte(0) || prefix == l.OpenDelimiter) ||
			(l.TokenSeparator == byte(0) || prefix == l.TokenSeparator)
	}
	for _, op := range l.UnaryOperators {
		inputCopy := expression
		inputOffset := 0
		for index := strings.Index(expression, op); index >= 0; index = strings.Index(inputCopy, op) {
			valid := (index == 0 || unaryOpValidatePrefix(inputCopy[index-1]))
			if valid {
				unaryOpIndices[index+inputOffset] = op
			}
			inputOffset += len(inputCopy[:index+len(op)])
			inputCopy = inputCopy[index+len(op):]
		}
	}

	tokens := []Token{}

	i := 0
	var operand strings.Builder
	operandType := Operand
	for i < len(expression) {
		foundType := false
		var token Token
		if op, ok := binaryOpIndices[i]; ok && operandType != StringOperand {
			token = Token{
				Value: op,
				Type:  BinaryOperator,
			}
			i += len(op)
			foundType = true
		} else if op, ok := binaryFuncIndices[i]; ok && operandType != StringOperand {
			token = Token{
				Value: op,
				Type:  BinaryFunc,
			}
			i += len(op)
			foundType = true
		} else if op, ok := unaryFuncIndices[i]; ok && operandType != StringOperand {
			token = Token{
				Value: op,
				Type:  UnaryFunc,
			}
			i += len(op)
			foundType = true
		} else if op, ok := unaryOpIndices[i]; ok && operandType != StringOperand {
			token = Token{
				Value: op,
				Type:  UnaryOperator,
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
		} else if expression[i] == l.BinaryFunctionOpSeparator && operandType != StringOperand {
			token = Token{
				Value: string(expression[i]),
				Type:  BinaryFuncSeparator,
			}
			i++
			foundType = true
		} else if ((l.TokenSeparator != byte(0) && expression[i] == l.TokenSeparator) || expression[i] == ' ') && operandType != StringOperand {
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
			if i >= len(expression) {
				tokens = append(tokens, Token{
					Value: operand.String(),
					Type:  operandType,
				})
			}
		}
	}

	return &TokenStream{Tokens: tokens}
}
