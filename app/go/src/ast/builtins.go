package ast

import "fmt"

var Builtins = []struct {
	Name    string
	Builtin *Builtin
}{
	{
		"len",
		&Builtin{Func: func(args ...Expression) Expression {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			switch arg := args[0].(type) {
			case *Array:
				return &Int{Value: len(arg.Exprs)}
			}
			return nil
		},
		},
	}, {
		"print",
		&Builtin{Func: func(args ...Expression) Expression {
			str := args[0].(*Array)
			out := ""
			for i := 0; i < len(str.Exprs); i++ {
				out += string(str.Exprs[i].(*Char).Value)
			}
			fmt.Println(out)
			return nil
		},
		},
	}, {
		"intToString",
		&Builtin{Func: func(e ...Expression) Expression {
			str := fmt.Sprintf("%d", e[0].(*Int).Value)
			out := &Array{}
			for i := 0; i < len(str); i++ {
				out.Exprs = append(out.Exprs, &Char{Value: str[i]})
			}
			return out
		},
		},
	},
}

type Error struct {
	Message string
}

func (e Error) isExpression()          {}
func (e Error) Type(_ []string) string { return "" }

func newError(format string, a ...interface{}) *Error {
	return &Error{Message: fmt.Sprintf(format, a...)}
}

func GetBuiltinByName(name string) *Builtin {
	for _, def := range Builtins {
		if def.Name == name {
			return def.Builtin
		}
	}
	return nil
}

var char Type = &PrimitiveType{"Char"}

var BuiltinVars = []struct {
	Name string
	Val  Expression
}{
	{
		Name: "Int",
		Val:  &PrimitiveType{"Int"},
	}, {
		Name: "Bool",
		Val:  &PrimitiveType{"Bool"},
	}, {
		Name: "Void",
		Val:  &PrimitiveType{"Void"},
	}, {
		Name: "Float",
		Val:  &PrimitiveType{"Float"},
	}, {
		Name: "Char",
		Val:  char,
	}, {
		Name: "String",
		Val:  &ArrayType{char},
	},
}
