package vm

import (
	"breeze.ryanbrewer.page/ast"
	"breeze.ryanbrewer.page/code"
)

type Frame struct {
	c           ast.Expression
	ip          int
	basePointer int
	Block       bool
}

func NewFrame(cl *ast.Closure, basePointer int) *Frame {
	return &Frame{
		c:           cl,
		ip:          -1,
		basePointer: basePointer,
		Block:       false,
	}
}

func (f *Frame) Instructions() code.Instructions {
	switch c := f.c.(type) {
	case *ast.Closure:
		return c.Fn.Instructions
	}
	return nil
}
