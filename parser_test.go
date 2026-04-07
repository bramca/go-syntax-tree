package syntaxtree

import (
	"testing"
)

func TestPrattParser_Parse_OperandOnly(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream   *TokenStream
		minPrecedence int
		expectedValue string
		expectedType  NodeType
		expectedLen   int
	}{
		"single operand": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "5", Type: Operand},
				},
			},
			minPrecedence: 0,
			expectedValue: "5",
			expectedType:  LeftOperand,
			expectedLen:   1,
		},
		"string operand": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "'hello'", Type: StringOperand},
				},
			},
			minPrecedence: 0,
			expectedValue: "'hello'",
			expectedType:  LeftOperand,
			expectedLen:   1,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{}}
			root, nodes, err := parser.Parse(tc.tokenStream, tc.minPrecedence, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedValue)
			Equal(t, root.Type, tc.expectedType)
			Equal(t, len(nodes), tc.expectedLen)
		})
	}
}

func TestPrattParser_Parse_BinaryOperator(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream        *TokenStream
		minPrecedence      int
		expectedRootValue  string
		expectedLeftValue  string
		expectedRightValue string
		expectedLen        int
	}{
		"simple addition": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "2", Type: Operand},
				},
			},
			minPrecedence:      0,
			expectedRootValue:  "+",
			expectedLeftValue:  "1",
			expectedRightValue: "2",
			expectedLen:        3,
		},
		"simple multiplication": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "3", Type: Operand},
					{Value: "*", Type: BinaryOperator},
					{Value: "4", Type: Operand},
				},
			},
			minPrecedence:      0,
			expectedRootValue:  "*",
			expectedLeftValue:  "3",
			expectedRightValue: "4",
			expectedLen:        3,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
			root, nodes, err := parser.Parse(tc.tokenStream, tc.minPrecedence, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			Equal(t, root.Type, Operator)
			Equal(t, root.LeftChild.Value, tc.expectedLeftValue)
			Equal(t, root.RightChild.Value, tc.expectedRightValue)
			Equal(t, len(nodes), tc.expectedLen)
		})
	}
}

func TestPrattParser_Parse_Precedence(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream        *TokenStream
		minPrecedence      int
		expectedRootValue  string
		expectedChildValue string
	}{
		"multiplication has higher precedence": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "2", Type: Operand},
					{Value: "*", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			minPrecedence:      0,
			expectedRootValue:  "+",
			expectedChildValue: "*",
		},
		"respects min precedence": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "2", Type: Operand},
					{Value: "*", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			minPrecedence:      2,
			expectedRootValue:  "1",
			expectedChildValue: "1",
		},
		"respects min precedence with matching precedence": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "2", Type: Operand},
					{Value: "*", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			minPrecedence:      1,
			expectedRootValue:  "+",
			expectedChildValue: "*",
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
			root, _, err := parser.Parse(tc.tokenStream, tc.minPrecedence, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			if tc.expectedRootValue != tc.expectedChildValue {
				Equal(t, root.RightChild.Value, tc.expectedChildValue)
			}
		})
	}
}

func TestPrattParser_Parse_LeftToRightAssociativity(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream                  *TokenStream
		expectedRootValue            string
		expectedRootLeftChildValue   string
		expectedRootRightChildValue  string
		expectedInnerLeftChildValue  string
		expectedInnerRightChildValue string
	}{
		"subtraction is left associative": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "-", Type: BinaryOperator},
					{Value: "2", Type: Operand},
					{Value: "-", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			expectedRootValue:            "-",
			expectedRootLeftChildValue:   "-",
			expectedRootRightChildValue:  "3",
			expectedInnerLeftChildValue:  "1",
			expectedInnerRightChildValue: "2",
		},
		"addition is left associative": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "2", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			expectedRootValue:            "+",
			expectedRootLeftChildValue:   "+",
			expectedRootRightChildValue:  "3",
			expectedInnerLeftChildValue:  "1",
			expectedInnerRightChildValue: "2",
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{"+": 1, "-": 1}}
			root, _, err := parser.Parse(tc.tokenStream, 0, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			Equal(t, root.LeftChild.Value, tc.expectedRootLeftChildValue)
			Equal(t, root.RightChild.Value, tc.expectedRootRightChildValue)
			Equal(t, root.LeftChild.LeftChild.Value, tc.expectedInnerLeftChildValue)
			Equal(t, root.LeftChild.RightChild.Value, tc.expectedInnerRightChildValue)
		})
	}
}

func TestPrattParser_Parse_Grouping(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream             *TokenStream
		expectedRootValue       string
		expectedLeftChildValue  string
		expectedRightChildValue string
		expectedIsGroup         bool
	}{
		"grouped expression": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "(", Type: OpenDelimiter},
					{Value: "1", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "2", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
					{Value: "*", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			expectedRootValue:       "*",
			expectedLeftChildValue:  "+",
			expectedRightChildValue: "3",
			expectedIsGroup:         true,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{"+": 1, "-": 1, "*": 2, "/": 2}}
			root, _, err := parser.Parse(tc.tokenStream, 0, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			Equal(t, root.LeftChild.Value, tc.expectedLeftChildValue)
			Equal(t, root.RightChild.Value, tc.expectedRightChildValue)
			Equal(t, root.LeftChild.IsGroup, tc.expectedIsGroup)
		})
	}
}

func TestPrattParser_Parse_BinaryFunction(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream             *TokenStream
		expectedRootValue       string
		expectedLeftChildValue  string
		expectedRightChildValue string
	}{
		"pow function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "pow", Type: BinaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "2", Type: Operand},
					{Value: ",", Type: BinaryFuncSeparator},
					{Value: "3", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
				},
			},
			expectedRootValue:       "pow",
			expectedLeftChildValue:  "2",
			expectedRightChildValue: "3",
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{}}
			root, _, err := parser.Parse(tc.tokenStream, 0, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			Equal(t, root.Type, Operator)
			Equal(t, root.LeftChild.Value, tc.expectedLeftChildValue)
			Equal(t, root.RightChild.Value, tc.expectedRightChildValue)
		})
	}
}

func TestPrattParser_Parse_UnaryFunction(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream        *TokenStream
		expectedRootValue  string
		expectedChildValue string
	}{
		"sqrt function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "sqrt", Type: UnaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "16", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
				},
			},
			expectedRootValue:  "sqrt",
			expectedChildValue: "16",
		},
		"nested unary functions": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "sqrt", Type: UnaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "sqrt", Type: UnaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "9", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
					{Value: ")", Type: CloseDelimiter},
				},
			},
			expectedRootValue:  "sqrt",
			expectedChildValue: "sqrt",
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{}}
			root, _, err := parser.Parse(tc.tokenStream, 0, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			Equal(t, root.Type, UnaryFunction)
			Equal(t, root.LeftChild.Value, tc.expectedChildValue)
		})
	}
}

func TestPrattParser_Parse_UnaryOperator(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream        *TokenStream
		expectedRootValue  string
		expectedChildValue string
	}{
		"negative number": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "-", Type: UnaryOperator},
					{Value: "5", Type: Operand},
				},
			},
			expectedRootValue:  "-",
			expectedChildValue: "5",
		},
		"unary operator in expression": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "-", Type: UnaryOperator},
					{Value: "5", Type: Operand},
					{Value: "+", Type: BinaryOperator},
					{Value: "3", Type: Operand},
				},
			},
			expectedRootValue:  "+",
			expectedChildValue: "-",
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{"+": 1}}
			root, _, err := parser.Parse(tc.tokenStream, 0, nil)

			NoError(t, err)
			Equal(t, root.Value, tc.expectedRootValue)
			Equal(t, root.LeftChild.Value, tc.expectedChildValue)
		})
	}
}

func TestPrattParser_Parse_Error(t *testing.T) {
	t.Parallel()
	tests := map[string]struct {
		tokenStream      *TokenStream
		expectedErrorMsg string
	}{
		"closing delimiter in prefix": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: ")", Type: CloseDelimiter},
					{Value: "1", Type: Operand},
				},
			},
			expectedErrorMsg: "failed to parse query: unexpected token: \")\" (CloseDelimiter)",
		},
		"missing closing delimiter in group": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "(", Type: OpenDelimiter},
					{Value: "1", Type: Operand},
				},
			},
			expectedErrorMsg: "failed to parse query: expected ')' but got \"\"",
		},
		"missing separator in binary function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "pow", Type: BinaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "2", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
				},
			},
			expectedErrorMsg: "failed to parse query: expected ',' in binary function pow, got \")\"",
		},
		"missing opening delimiter in binary function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "pow", Type: BinaryFunc},
					{Value: "2", Type: Operand},
					{Value: ",", Type: BinaryFuncSeparator},
					{Value: "3", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
				},
			},
			expectedErrorMsg: "failed to parse query: expected '(' after binary function pow, got \"2\"",
		},
		"missing closing delimiter in binary function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "pow", Type: BinaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "2", Type: Operand},
					{Value: ",", Type: BinaryFuncSeparator},
					{Value: "3", Type: Operand},
				},
			},
			expectedErrorMsg: "failed to parse query: expected ')' after binary function pow, got \"\"",
		},
		"missing opening delimiter in unary function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "sqrt", Type: UnaryFunc},
					{Value: "16", Type: Operand},
					{Value: ")", Type: CloseDelimiter},
				},
			},
			expectedErrorMsg: "failed to parse query: expected '(' after unary function sqrt, got \"16\"",
		},
		"missing closing delimiter in unary function": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "sqrt", Type: UnaryFunc},
					{Value: "(", Type: OpenDelimiter},
					{Value: "16", Type: Operand},
				},
			},
			expectedErrorMsg: "failed to parse query: expected ')' after unary function sqrt, got \"\"",
		},
		"unknown operator in precedence table": {
			tokenStream: &TokenStream{
				Tokens: []Token{
					{Value: "1", Type: Operand},
					{Value: "?", Type: BinaryOperator},
					{Value: "2", Type: Operand},
				},
			},
			expectedErrorMsg: "failed to parse query: token \"?\" not in precedence table",
		},
		"typo in query": {
			tokenStream: &TokenStream{Tokens: []Token{
				{Value: "conct", Type: Operand},
				{Value: "(", Type: OpenDelimiter},
				{Value: "'#'", Type: StringOperand},
				{Value: ",", Type: BinaryFuncSeparator},
				{Value: "name", Type: Operand},
				{Value: ")", Type: CloseDelimiter},
				{Value: "eq", Type: BinaryOperator},
				{Value: "'#test'", Type: StringOperand},
			}},
			expectedErrorMsg: "failed to parse query: unexpected token \"(\" (OpenDelimiter) after \"conct\" (LeftOperand)",
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			parser := PrattParser{Precendence: map[string]int{}}
			_, _, err := parser.Parse(tc.tokenStream, 0, nil)

			Error(t, err)
			Equal(t, err.Error(), tc.expectedErrorMsg)
		})
	}
}

func TestPrattParser_Parse_CloseDelimiterStopsParsing(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "1", Type: Operand},
			{Value: "+", Type: BinaryOperator},
			{Value: "2", Type: Operand},
			{Value: ")", Type: CloseDelimiter},
			{Value: "*", Type: BinaryOperator},
			{Value: "3", Type: Operand},
		},
	}
	parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
	root, _, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, root.Value, "+")
	Equal(t, root.RightChild.Value, "2")
}

func TestPrattParser_Parse_EOFStopsParsing(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "5", Type: Operand},
			{Value: "", Type: EOF},
			{Value: "+", Type: Operand},
			{Value: "4", Type: Operand},
		},
	}
	parser := PrattParser{Precendence: map[string]int{}}
	root, _, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, root.Value, "5")
}

func TestPrattParser_Parse_NodeIdsAreSequential(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "1", Type: Operand},
			{Value: "+", Type: BinaryOperator},
			{Value: "2", Type: Operand},
			{Value: "*", Type: BinaryOperator},
			{Value: "3", Type: Operand},
		},
	}
	parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
	_, nodes, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, len(nodes), 5)
	seen := make(map[int]bool)
	for _, n := range nodes {
		Equal(t, seen[n.Id], false)
		seen[n.Id] = true
	}
	for i := range 5 {
		Equal(t, seen[i], true)
	}
}

func TestPrattParser_Parse_ParentChildRelationships(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "1", Type: Operand},
			{Value: "+", Type: BinaryOperator},
			{Value: "2", Type: Operand},
			{Value: "*", Type: BinaryOperator},
			{Value: "3", Type: Operand},
		},
	}
	parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
	root, _, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, root.Parent, (*Node)(nil))
	Equal(t, root.LeftChild.Parent, root)
	Equal(t, root.RightChild.Parent, root)
	Equal(t, root.RightChild.LeftChild.Parent, root.RightChild)
	Equal(t, root.RightChild.RightChild.Parent, root.RightChild)
}

func TestPrattParser_Parse_ComplexNestedExpression(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "1", Type: Operand},
			{Value: "+", Type: BinaryOperator},
			{Value: "sqrt", Type: UnaryFunc},
			{Value: "(", Type: OpenDelimiter},
			{Value: "4", Type: Operand},
			{Value: ")", Type: CloseDelimiter},
			{Value: "*", Type: BinaryOperator},
			{Value: "pow", Type: BinaryFunc},
			{Value: "(", Type: OpenDelimiter},
			{Value: "2", Type: Operand},
			{Value: ",", Type: BinaryFuncSeparator},
			{Value: "3", Type: Operand},
			{Value: ")", Type: CloseDelimiter},
		},
	}
	parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
	root, _, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, root.Value, "+")
	Equal(t, root.LeftChild.Value, "1")
	Equal(t, root.RightChild.Value, "*")
	Equal(t, root.RightChild.LeftChild.Value, "sqrt")
	Equal(t, root.RightChild.LeftChild.Type, UnaryFunction)
	Equal(t, root.RightChild.RightChild.Value, "pow")
	Equal(t, root.RightChild.RightChild.Type, Operator)
}

func TestPrattParser_Parse_BinaryFunctionWithOperator(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "pow", Type: BinaryFunc},
			{Value: "(", Type: OpenDelimiter},
			{Value: "2", Type: Operand},
			{Value: ",", Type: BinaryFuncSeparator},
			{Value: "3", Type: Operand},
			{Value: ")", Type: CloseDelimiter},
			{Value: "+", Type: BinaryOperator},
			{Value: "1", Type: Operand},
		},
	}
	parser := PrattParser{Precendence: map[string]int{"+": 1}}
	root, _, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, root.Value, "+")
	Equal(t, root.LeftChild.Value, "pow")
	Equal(t, root.LeftChild.Type, Operator)
	Equal(t, root.RightChild.Value, "1")
}

func TestPrattParser_Parse_BinaryFunctionInGroup(t *testing.T) {
	t.Parallel()
	tokenStream := &TokenStream{
		Tokens: []Token{
			{Value: "(", Type: OpenDelimiter},
			{Value: "pow", Type: BinaryFunc},
			{Value: "(", Type: OpenDelimiter},
			{Value: "2", Type: Operand},
			{Value: ",", Type: BinaryFuncSeparator},
			{Value: "3", Type: Operand},
			{Value: ")", Type: CloseDelimiter},
			{Value: "+", Type: BinaryOperator},
			{Value: "1", Type: Operand},
			{Value: ")", Type: CloseDelimiter},
			{Value: "*", Type: BinaryOperator},
			{Value: "2", Type: Operand},
		},
	}
	parser := PrattParser{Precendence: map[string]int{"+": 1, "*": 2}}
	root, _, err := parser.Parse(tokenStream, 0, nil)

	NoError(t, err)
	Equal(t, root.Value, "*")
	Equal(t, root.RightChild.Value, "2")
	Equal(t, root.LeftChild.Value, "+")
	Equal(t, root.LeftChild.IsGroup, true)
}
