package syntaxtree

import "fmt"

type ParseError struct {
	Msg string
}

func (p *ParseError) Error() string {
	return fmt.Sprintf("failed to parse query: %s", p.Msg)
}
