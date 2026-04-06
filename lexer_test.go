package syntaxtree

import (
	"testing"
)

var (
	mathLexer = &Lexer{
		BinaryOperators: []string{
			"*",
			"/",
			"+",
			"-",
		},
		UnaryOperators: []string{
			"-",
		},
		BinaryFunctions: []string{
			"pow",
		},
		UnaryFunctions: []string{
			"sqrt",
		},
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		BinaryFunctionOpSeparator: ',',
	}

	odataLexer = &Lexer{
		BinaryOperators: []string{
			"eq",
			"ne",
			"and",
			"or",
		},
		BinaryFunctions: []string{
			"contains",
		},
		UnaryFunctions: []string{
			"tolower",
			"not",
		},
		OpenDelimiter:             '(',
		CloseDelimiter:            ')',
		StringDelimiter:           '\'',
		BinaryFunctionOpSeparator: ',',
		TokenSeparator:            ' ',
	}
)

func TestLexer_TokenType_CorrectString(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenType      TokenType
		expectedResult string
	}{
		"unknown": {
			tokenType:      EOF,
			expectedResult: "Unknown",
		},
		"operand": {
			tokenType:      Operand,
			expectedResult: "Operand",
		},
		"stringOperand": {
			tokenType:      StringOperand,
			expectedResult: "StringOperand",
		},
		"binaryOperator": {
			tokenType:      BinaryOperator,
			expectedResult: "BinaryOperator",
		},
		"unaryOperator": {
			tokenType:      UnaryOperator,
			expectedResult: "UnaryOperator",
		},
		"binaryFunc": {
			tokenType:      BinaryFunc,
			expectedResult: "BinaryFunc",
		},
		"unaryFunc": {
			tokenType:      UnaryFunc,
			expectedResult: "UnaryFunc",
		},
		"openDelimiter": {
			tokenType:      OpenDelimiter,
			expectedResult: "OpenDelimiter",
		},
		"closeDelimiter": {
			tokenType:      CloseDelimiter,
			expectedResult: "CloseDelimiter",
		},
		"binaryFuncSeparator": {
			tokenType:      BinaryFuncSeparator,
			expectedResult: "BinaryFuncSeparator",
		},
	}

	for name, testData := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			// Act
			result := testData.tokenType.String()

			// Assert
			Equal(t, result, testData.expectedResult)
		})
	}

}

func TestLexer_TokenStreamNext_ReturnsCorrectToken(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream        *TokenStream
		expectedToken      Token
		expectedTokensLeft []Token
	}{
		"no token": {
			tokenStream: &TokenStream{
				Tokens: []Token{},
			},
			expectedToken: Token{
				Value: "",
				Type:  EOF,
			},
			expectedTokensLeft: []Token{},
		},
		"single token": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{
						Value: "1",
						Type:  Operand,
					},
				},
			},
			expectedToken: Token{
				Value: "1",
				Type:  Operand,
			},
			expectedTokensLeft: []Token{},
		},
		"multiple tokens": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{
						Value: "1",
						Type:  Operand,
					},
					{
						Value: "+",
						Type:  BinaryOperator,
					},
					{
						Value: "2",
						Type:  Operand,
					},
				},
			},
			expectedToken: Token{
				Value: "2",
				Type:  Operand,
			},
			expectedTokensLeft: []Token{

				{
					Value: "1",
					Type:  Operand,
				},
				{
					Value: "+",
					Type:  BinaryOperator,
				},
			},
		},
	}

	for name, data := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			// Act
			token := data.tokenStream.Next()

			// Assert
			Equal(t, data.expectedToken, token)
			Equal(t, len(data.expectedTokensLeft), len(data.tokenStream.Tokens))
			for i := range data.tokenStream.Tokens {
				Equal(t, data.expectedTokensLeft[i], data.tokenStream.Tokens[i])
			}
		})
	}
}

func TestLexer_TokenStreamPeek_ReturnsCorrectToken(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream        *TokenStream
		expectedToken      Token
		expectedTokensLeft []Token
	}{
		"no token": {
			tokenStream: &TokenStream{
				Tokens: []Token{},
			},
			expectedToken: Token{
				Value: "",
				Type:  EOF,
			},
			expectedTokensLeft: []Token{},
		},
		"single token": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{
						Value: "1",
						Type:  Operand,
					},
				},
			},
			expectedToken: Token{
				Value: "1",
				Type:  Operand,
			},
			expectedTokensLeft: []Token{
				{
					Value: "1",
					Type:  Operand,
				},
			},
		},
		"multiple tokens": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{
						Value: "1",
						Type:  Operand,
					},
					{
						Value: "+",
						Type:  BinaryOperator,
					},
					{
						Value: "2",
						Type:  Operand,
					},
				},
			},
			expectedToken: Token{
				Value: "2",
				Type:  Operand,
			},
			expectedTokensLeft: []Token{
				{
					Value: "1",
					Type:  Operand,
				},
				{
					Value: "+",
					Type:  BinaryOperator,
				},
				{
					Value: "2",
					Type:  Operand,
				},
			},
		},
	}

	for name, data := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			// Act
			token := data.tokenStream.Peek()

			// Assert
			Equal(t, data.expectedToken, token)
			Equal(t, len(data.expectedTokensLeft), len(data.tokenStream.Tokens))
			for i := range data.tokenStream.Tokens {
				Equal(t, data.expectedTokensLeft[i], data.tokenStream.Tokens[i])
			}
		})
	}
}

func TestLexer_Tokenize_ReturnCorrectTokens(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		query          string
		lexer          *Lexer
		expectedTokens []Token
	}{
		"math simple example": {
			query: "1+2*3",
			lexer: mathLexer,
			expectedTokens: []Token{
				{Value: "1", Type: Operand},
				{Value: "+", Type: BinaryOperator},
				{Value: "2", Type: Operand},
				{Value: "*", Type: BinaryOperator},
				{Value: "3", Type: Operand},
			},
		},
		"math example": {
			query: "-1+pow(2+3*4,pow((-1+sqrt(3))*4,3))",
			lexer: mathLexer,
			expectedTokens: []Token{
				{Value: "-", Type: UnaryOperator},
				{Value: "1", Type: Operand},
				{Value: "+", Type: BinaryOperator},
				{Value: "pow", Type: BinaryFunc},
				{Value: "(", Type: OpenDelimiter},
				{Value: "2", Type: Operand},
				{Value: "+", Type: BinaryOperator},
				{Value: "3", Type: Operand},
				{Value: "*", Type: BinaryOperator},
				{Value: "4", Type: Operand},
				{Value: ",", Type: BinaryFuncSeparator},
				{Value: "pow", Type: BinaryFunc},
				{Value: "(", Type: OpenDelimiter},
				{Value: "(", Type: OpenDelimiter},
				{Value: "-", Type: UnaryOperator},
				{Value: "1", Type: Operand},
				{Value: "+", Type: BinaryOperator},
				{Value: "sqrt", Type: UnaryFunc},
				{Value: "(", Type: OpenDelimiter},
				{Value: "3", Type: Operand},
				{Value: ")", Type: CloseDelimiter},
				{Value: ")", Type: CloseDelimiter},
				{Value: "*", Type: BinaryOperator},
				{Value: "4", Type: Operand},
				{Value: ",", Type: BinaryFuncSeparator},
				{Value: "3", Type: Operand},
				{Value: ")", Type: CloseDelimiter},
				{Value: ")", Type: CloseDelimiter},
			},
		},
		"odata example": {
			query: "name eq 'test' or anequivalent eq 'name eq contains' or contains(tolower(name), 'contains(not(an), edgecase)')",
			lexer: odataLexer,
			expectedTokens: []Token{
				{Value: "name", Type: Operand},
				{Value: "eq", Type: BinaryOperator},
				{Value: "'test'", Type: StringOperand},
				{Value: "or", Type: BinaryOperator},
				{Value: "anequivalent", Type: Operand},
				{Value: "eq", Type: BinaryOperator},
				{Value: "'name eq contains'", Type: StringOperand},
				{Value: "or", Type: BinaryOperator},
				{Value: "contains", Type: BinaryFunc},
				{Value: "(", Type: OpenDelimiter},
				{Value: "tolower", Type: UnaryFunc},
				{Value: "(", Type: OpenDelimiter},
				{Value: "name", Type: Operand},
				{Value: ")", Type: CloseDelimiter},
				{Value: ",", Type: BinaryFuncSeparator},
				{Value: "'contains(not(an), edgecase)'", Type: StringOperand},
				{Value: ")", Type: CloseDelimiter},
			},
		},
	}

	for name, data := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()

			// Act
			tokenStream := data.lexer.Tokenize(data.query)

			// Assert
			Equal(t, len(data.expectedTokens), len(tokenStream.Tokens))
			for i := range tokenStream.Tokens {
				Equal(t, data.expectedTokens[i], tokenStream.Tokens[i])
			}
		})
	}
}
