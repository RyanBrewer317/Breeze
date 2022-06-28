package ast

import (
	"fmt"
	"strconv"

	"breeze.ryanbrewer.page/code"
)

type Statement interface {
	isStatement()
}

type Expression interface {
	isExpression()
	Type([]string) string
}

type Assignment struct {
	Name  string
	Value Expression
}

func (a Assignment) isStatement() {}

type DotAssignment struct {
	Struct Expression
	Field  string
	Value  Expression
}

func (da DotAssignment) isStatement() {}

type IndexAssignment struct {
	Array Expression
	Index Expression
	Value Expression
}

func (ia IndexAssignment) isStatement() {}

type FunctionDeclaration struct {
	Name  string
	Args  []Ident
	Block Block
}

func (fd FunctionDeclaration) isStatement() {}

type Block struct {
	Statements []Statement
}

func (b Block) isStatement() {}

type If struct {
	Cond  Expression
	Block Statement
}

func (i If) isStatement() {}

type IfElse struct {
	Cond Expression
	Cons Statement
	Alt  Statement
}

func (ie IfElse) isStatement() {}

type For struct {
	Init  Statement
	Cond  Expression
	Iter  Statement
	Block Statement
}

func (f For) isStatement() {}

type While struct {
	Cond  Expression
	Block Statement
}

func (w While) isStatement() {}

type Execution struct {
	Expr Expression
}

func (e Execution) isStatement() {}

type Return struct {
	Expr Expression
}

func (r Return) isStatement() {}

type Using struct {
	Idents []string
}

func (u Using) isStatement() {}

type Switch struct {
	Expr    Expression
	Cases   map[Expression]Statement
	Default Statement
}

func (s Switch) isStatement() {}

type TypeSwitch struct {
	Expr    Expression
	Cases   map[Expression]Statement
	Default Statement
}

func (ts TypeSwitch) isStatement() {}

type Break struct{}

func (b Break) isStatement() {}

type Null struct{}

func (n Null) isExpression() {}
func (n Null) Type(_ []string) string {
	return "Void"
}

type Bool struct {
	Value bool
}

func (b Bool) isExpression() {}
func (b Bool) Type(_ []string) string {
	return "Bool"
}

type Ident struct {
	Name    string
	TypeInt int
}

func (i Ident) isExpression() {}
func (i Ident) Type(typeList []string) string {
	return typeList[i.TypeInt]
}

type Int struct {
	Value int
}

func (i Int) isExpression() {}
func (i Int) Type(_ []string) string {
	return "Int"
}

type Float struct {
	Value float64
}

func (f Float) isExpression() {}
func (f Float) Type(_ []string) string {
	return "Float"
}

type Char struct {
	Value byte
}

func (c Char) isExpression() {}
func (c Char) Type(_ []string) string {
	return "Char"
}

type Index struct {
	Collection Expression
	Value      Expression
	TypeInt    int
}

func (i Index) isExpression() {}
func (i Index) Type(typeList []string) string {
	return typeList[i.TypeInt]
}

type Array struct {
	TypeInt int
	Exprs   []Expression
}

func (a Array) isExpression() {}
func (a Array) Type(typeList []string) string {
	return typeList[a.TypeInt]
}

type Ternary struct {
	TypeInt int
	Cond    Expression
	Cons    Expression
	Alt     Expression
}

func (t Ternary) isExpression() {}
func (t Ternary) Type(typeList []string) string {
	return typeList[t.TypeInt]
}

type Tuple struct {
	Exprs []Expression
}

func (t Tuple) isExpression() {}
func (t Tuple) Type(typeList []string) string {
	out := ""
	for i := 0; i < len(t.Exprs); i++ {
		out += "*" + t.Exprs[i].Type(typeList)
	}
	return out[1:]
}

type Call struct {
	Func    Expression
	Arg     Tuple
	TypeInt int
}

func (c Call) isExpression() {}
func (c Call) Type(typeList []string) string {
	return typeList[c.TypeInt]
}

type Infix struct {
	TypeInt int
	Left    Expression
	Op      string
	Right   Expression
}

func (i Infix) isExpression() {}
func (i Infix) Type(typeList []string) string {
	return typeList[i.TypeInt]
}

type Prefix struct {
	TypeInt int
	Op      string
	Right   Expression
}

func (p Prefix) isExpression() {}
func (p Prefix) Type(typeList []string) string {
	return typeList[p.TypeInt]
}

type Struct struct {
	TypeInt int
	Names   []string
	Values  []Expression
}

func (s Struct) isExpression() {}
func (s Struct) Type(typeList []string) string {
	return typeList[s.TypeInt]
}

type Dot struct {
	TypeInt   int
	Obj       Expression
	FieldName string
}

func (d Dot) isExpression() {}
func (d Dot) Type(typeList []string) string {
	return typeList[d.TypeInt]
}

type Type interface {
	Expression
	String() string
}

type PrimitiveType struct {
	Name string
}

func (pt PrimitiveType) isExpression()          {}
func (pt PrimitiveType) Type(_ []string) string { return "Type" }
func (pt PrimitiveType) String() string         { return pt.Name }

type ArrayType struct {
	InnerType Type
}

func (at ArrayType) isExpression()          {}
func (at ArrayType) Type(_ []string) string { return "Type" }
func (at ArrayType) String() string         { return "[]" + at.InnerType.String() }

type ProductType struct {
	Left  Type
	Right Type
}

func (pt ProductType) isExpression()          {}
func (pt ProductType) Type(_ []string) string { return "Type" }
func (pt ProductType) String() string         { return pt.Left.String() + "*" + pt.Right.String() }

type SumType struct {
	Left  Type
	Right Type
}

func (st SumType) isExpression()          {}
func (st SumType) Type(_ []string) string { return "Type" }
func (st SumType) String() string         { return st.Left.String() + "+" + st.Right.String() }

type FunctionType struct {
	ArgType Type
	RetType Type
}

func (ft FunctionType) isExpression()          {}
func (ft FunctionType) Type(_ []string) string { return "Type" }
func (ft FunctionType) String() string         { return ft.ArgType.String() + "->" + ft.RetType.String() }

type StructType struct {
	Types []Expression
	Names []string
}

func (st StructType) isExpression()          {}
func (st StructType) Type(_ []string) string { return "Type" }
func (st StructType) String() string {
	out := "struct{"
	for i := 0; i < len(st.Types); i++ {
		out += st.Types[i].(Type).String() + " " + st.Names[i] + ", "
	}
	if len(st.Types) == 0 {
		out += ", "
	}
	return out[:len(out)-2] + "}"
}

type InterfaceType struct {
	Types []Expression
	Names []string
}

func (it InterfaceType) isExpression()          {}
func (it InterfaceType) Type(_ []string) string { return "Type" }
func (it InterfaceType) String() string {
	out := "interface{"
	for i := 0; i < len(it.Types); i++ {
		out += it.Types[i].(Type).String() + " " + it.Names[i] + ", "
	}
	if len(it.Types) == 0 {
		out += ", "
	}
	return out[:len(out)-2] + "}"
}

func ToString(expr Expression) string {
	switch expr := expr.(type) {
	case *Null:
		return "null"
	case *Bool:
		if expr.Value {
			return "true"
		}
		return "false"
	case *Int:
		return fmt.Sprintf("%d", expr.Value)
	case *Float:
		return strconv.FormatFloat(expr.Value, 'f', -1, 64)
	case *Char:
		return "'" + string(expr.Value) + "'"
	case *Array:
		isString := true
		out := "\""
		for _, e := range expr.Exprs {
			c, ok := e.(*Char)
			if !ok {
				isString = false
				break
			}
			out += string(c.Value)
		}
		if isString {
			return out + "\""
		}
		out = "["
		for _, e := range expr.Exprs {
			out += ToString(e) + ", "
		}
		return out[:len(out)-2] + "]"
	}
	return ""
}

type Closure struct {
	Fn   *CompiledFunction
	Free []Expression
}

func (c Closure) isExpression() {}
func (c Closure) Type(typeList []string) string {
	return c.Fn.Type(typeList)
}

type CompiledFunction struct {
	TypeString    string
	Instructions  code.Instructions
	NumLocals     int
	NumParameters int
}

func (cf CompiledFunction) isExpression() {}
func (cf CompiledFunction) Type(_ []string) string {
	return cf.TypeString
}

type Builtin struct {
	TypeString string
	Func       func(...Expression) Expression
}

func (b Builtin) isExpression() {}
func (b Builtin) Type(_ []string) string {
	return b.TypeString
}

type JumpTable struct {
	Table   map[Expression]int
	Default int
}

func (jt JumpTable) isExpression() {}
func (jt JumpTable) Type(_ []string) string {
	return ""
}

type TypeJumpTable struct {
	Table   map[string]int
	Default int
}

func (tjt TypeJumpTable) isExpression() {}
func (tjt TypeJumpTable) Type(_ []string) string {
	return ""
}
