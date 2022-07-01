package parser

import (
	"log"
	"strconv"

	"breeze.ryanbrewer.page/ast"
)

func ParseExpression(inp string) (ast.Expression, int) {
	if inp == "" {
		return ast.Null{}, 0
	}
	switch inp[0] {
	case 'n':
		return ast.Null{}, 4
	case 't':
		return ast.Bool{Value: true}, 4
	case 'f':
		return ast.Bool{Value: false}, 5
	case '(':
		name := getOperand(inp[1:])
		typeInt_str := getOperand(inp[2+len(name):])
		typeInt, _ := strconv.Atoi(typeInt_str)
		return ast.Ident{Name: name, TypeInt: typeInt}, len(name) + len(typeInt_str) + 2
	case '#':
		n_str := getOperand(inp[1:])
		n, _ := strconv.Atoi(n_str)
		return ast.Int{Value: n}, len(n_str) + 1
	case '/':
		n_str := getOperand(inp[1:])
		n, _ := strconv.ParseFloat(n_str, 64)
		return ast.Float{Value: n}, len(n_str) + 1
	case '\'':
		return ast.Char{Value: inp[1]}, 3
	case '[':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		left, l1 := ParseExpression(inp[2+len(fullTypeInt_str):])
		right, l2 := ParseExpression(inp[l1+3+len(fullTypeInt_str):])
		return ast.Index{Collection: left, Value: right, TypeInt: fullTypeInt}, 3 + l1 + l2 + len(fullTypeInt_str)
	case ':':
		typeInt_str := getOperand(inp[1:])
		typeInt, _ := strconv.Atoi(typeInt_str)
		l_str := getOperand(inp[2+len(typeInt_str):])
		l, _ := strconv.Atoi(l_str)
		exprs := make([]ast.Expression, 0)
		col := 3 + len(typeInt_str) + len(l_str)
		for i := 0; i < l; i++ {
			expr, c := ParseExpression(inp[col:])
			exprs = append(exprs, expr)
			col += c + 1
		}
		if l == 0 {
			col++
		}
		return ast.Array{Exprs: exprs, TypeInt: typeInt}, col - 1
	case '*':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		exprs := make([]ast.Expression, 0)
		col := 2 + len(l_str)
		for i := 0; i < l; i++ {
			expr, c := ParseExpression(inp[col:])
			exprs = append(exprs, expr)
			col += c + 1
		}
		if l == 0 {
			col++
		}
		return ast.Tuple{Exprs: exprs}, col - 1
	case '?':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		cond, c1 := ParseExpression(inp[2+len(fullTypeInt_str):])
		cons, c2 := ParseExpression(inp[3+c1+len(fullTypeInt_str):])
		alt, c3 := ParseExpression(inp[4+c1+c2+len(fullTypeInt_str):])
		return ast.Ternary{Cond: cond, Cons: cons, Alt: alt, TypeInt: fullTypeInt}, 4 + c1 + c2 + c3 + len(fullTypeInt_str)
	case '\\':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		fun, c1 := ParseExpression(inp[2+len(fullTypeInt_str):])
		arg, c2 := ParseExpression(inp[3+c1+len(fullTypeInt_str):])
		return ast.Call{Func: fun, Arg: arg.(ast.Tuple), TypeInt: fullTypeInt}, 3 + c1 + c2 + len(fullTypeInt_str)
	case '-':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		op := getOperand(inp[2+len(fullTypeInt_str):])
		left, c1 := ParseExpression(inp[3+len(op)+len(fullTypeInt_str):])
		right, c2 := ParseExpression(inp[4+len(op)+c1+len(fullTypeInt_str):])
		return ast.Infix{Op: op, Left: left, Right: right, TypeInt: fullTypeInt}, 4 + len(op) + c1 + c2 + len(fullTypeInt_str)
	case '~':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		op := getOperand(inp[2+len(fullTypeInt_str):])
		right, c := ParseExpression(inp[3+len(op)+len(fullTypeInt_str):])
		return ast.Prefix{Op: op, Right: right, TypeInt: fullTypeInt}, 3 + len(op) + c + len(fullTypeInt_str)
	case '$':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		col := 2 + len(l_str)
		types := make([]ast.Expression, 0)
		names := make([]string, 0)
		for i := 0; i < l; i++ {
			typeExpr, c := ParseExpression(inp[col:])
			types = append(types, typeExpr)
			name := getOperand(inp[col+c+1:])
			names = append(names, name)
			col += c + 2 + len(name)
		}
		if l == 0 {
			col++
		}
		return ast.StructType{Types: types, Names: names}, col - 1
	case '}':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		col := 2 + len(l_str)
		types := make([]ast.Expression, 0)
		names := make([]string, 0)
		for i := 0; i < l; i++ {
			typeExpr, c := ParseExpression(inp[col:])
			types = append(types, typeExpr)
			name := getOperand(inp[col+c+1:])
			names = append(names, name)
			col += c + 2 + len(name)
		}
		if l == 0 {
			col++
		}
		return ast.InterfaceType{Types: types, Names: names}, col - 1
	case '{':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		l_str := getOperand(inp[2+len(fullTypeInt_str):])
		l, _ := strconv.Atoi(l_str)
		col := 3 + len(l_str) + len(fullTypeInt_str)
		names := make([]string, 0)
		values := make([]ast.Expression, 0)
		for i := 0; i < l; i++ {
			name := getOperand(inp[col:])
			val, c := ParseExpression(inp[col+1+len(name):])
			names = append(names, name)
			values = append(values, val)
			col += 2 + c + len(name)
		}
		if l == 0 {
			col++
		}
		return ast.Struct{TypeInt: fullTypeInt, Names: names, Values: values}, col - 1
	case '.':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		obj, c := ParseExpression(inp[2+len(fullTypeInt_str):])
		name := getOperand(inp[3+c+len(fullTypeInt_str):])
		return ast.Dot{TypeInt: fullTypeInt, Obj: obj, FieldName: name}, 3 + c + len(name) + len(fullTypeInt_str)
	case 'l':
		fullTypeInt_str := getOperand(inp[1:])
		fullTypeInt, _ := strconv.Atoi(fullTypeInt_str)
		argsExpr, c := ParseExpression(inp[2+len(fullTypeInt_str):])
		argsTuple := argsExpr.(ast.Tuple)
		args := make([]ast.Ident, len(argsTuple.Exprs))
		for i := 0; i < len(argsTuple.Exprs); i++ {
			args[i] = argsTuple.Exprs[i].(ast.Ident)
		}
		block, c2 := ParseStatement(inp[3+len(fullTypeInt_str)+c:])
		return ast.Lambda{TypeInt: fullTypeInt, Args: args, Block: block.(ast.Block)}, 3 + len(fullTypeInt_str) + c + c2
	}
	return nil, 0
}

func ParseStatement(inp string) (ast.Statement, int) {
	if inp == "" {
		return ast.Block{}, 0
	}
	switch inp[0] {
	case '=':
		name := getOperand(inp[1:])
		val, c := ParseExpression(inp[2+len(name):])
		return ast.Assignment{Name: name, Value: val}, 2 + len(name) + c
	case '.':
		obj, c := ParseExpression(inp[1:])
		field := getOperand(inp[2+c:])
		val, c2 := ParseExpression(inp[3+c+len(field):])
		return ast.DotAssignment{Struct: obj, Field: field, Value: val}, c + c2 + len(field) + 3
	case '[':
		arr, c := ParseExpression(inp[1:])
		idx, c2 := ParseExpression(inp[2+c:])
		val, c3 := ParseExpression(inp[3+c+c2:])
		return ast.IndexAssignment{Array: arr, Index: idx, Value: val}, 3 + c + c2 + c3
	case '\\':
		name := getOperand(inp[1:])
		l_str := getOperand(inp[2+len(name):])
		l, _ := strconv.Atoi(l_str)
		exprs := make([]ast.Ident, 0)
		col := 3 + len(name) + len(l_str)
		for i := 0; i < l; i++ {
			expr, c := ParseExpression(inp[col:])
			exprs = append(exprs, expr.(ast.Ident))
			col += c + 1
		}
		if l == 0 {
			col++
		}
		block, c := ParseStatement(inp[col:])
		return ast.FunctionDeclaration{Name: name, Args: exprs, Block: block.(ast.Block)}, col + c
	case '{':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		stmts := make([]ast.Statement, 0)
		col := 2 + len(l_str)
		for i := 0; i < l; i++ {
			stmt, c := ParseStatement(inp[col:])
			stmts = append(stmts, stmt)
			col += c + 1
		}
		if l == 0 {
			col++
		}
		return ast.Block{Statements: stmts}, col - 1
	case '?':
		cond, c1 := ParseExpression(inp[1:])
		cons, c2 := ParseStatement(inp[2+c1:])
		return ast.If{Cond: cond, Block: cons}, 2 + c1 + c2
	case ':':
		cond, c1 := ParseExpression(inp[1:])
		cons, c2 := ParseStatement(inp[2+c1:])
		alt, c3 := ParseStatement(inp[3+c1+c2:])
		return ast.IfElse{Cond: cond, Cons: cons, Alt: alt}, 3 + c1 + c2 + c3
	case '|':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		expr, c1 := ParseExpression(inp[2+len(l_str):])
		col := 3 + len(l_str) + c1
		cases := make(map[ast.Expression]ast.Statement)
		for i := 0; i < l; i++ {
			cas, c2 := ParseExpression(inp[col:])
			block, c3 := ParseStatement(inp[col+c2+1:])
			col += 2 + c2 + c3
			cases[cas] = block
		}
		if l > 0 {
			col--
		}
		var def ast.Statement
		c := 0
		if inp[col+1] != ' ' {
			def, c = ParseStatement(inp[col+1:])
		} else {
			def, c = nil, 0
		}
		return ast.Switch{Expr: expr, Cases: cases, Default: def}, col + c + 1
	case 'T':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		expr, c1 := ParseExpression(inp[2+len(l_str):])
		col := 3 + len(l_str) + c1
		cases := make(map[ast.Expression]ast.Statement)
		for i := 0; i < l; i++ {
			cas, c2 := ParseExpression(inp[col:])
			block, c3 := ParseStatement(inp[col+c2+1:])
			col += 2 + c2 + c3
			cases[cas] = block
		}
		if l > 0 {
			col--
		}
		var def ast.Statement
		c := 0
		if inp[col+1] != ' ' {
			def, c = ParseStatement(inp[col+1:])
		} else {
			def, c = nil, 0
		}
		return ast.TypeSwitch{Expr: expr, Cases: cases, Default: def}, col + c + 1
	case '#':
		init, c1 := ParseStatement(inp[1:])
		cond, c2 := ParseExpression(inp[2+c1:])
		if c2 == 0 {
			cond = ast.Bool{Value: true}
		}
		iter, c3 := ParseStatement(inp[3+c1+c2:])
		block, c4 := ParseStatement(inp[4+c1+c2+c3:])
		return ast.For{Init: init, Cond: cond, Iter: iter, Block: block}, 4 + c1 + c2 + c3 + c4
	case '@':
		cond, c1 := ParseExpression(inp[1:])
		block, c2 := ParseStatement(inp[2+c1:])
		return ast.While{Cond: cond, Block: block}, 2 + c1 + c2
	case '!':
		expr, c := ParseExpression(inp[1:])
		return ast.Execution{Expr: expr}, 1 + c
	case '^':
		expr, c := ParseExpression(inp[1:])
		return ast.Return{Expr: expr}, 1 + c
	case 'u':
		l_str := getOperand(inp[1:])
		l, _ := strconv.Atoi(l_str)
		idents := make([]string, 0)
		col := 2 + len(l_str)
		for i := 0; i < l; i++ {
			ident := getOperand(inp[col:])
			idents = append(idents, ident)
			col += len(ident) + 1
		}
		if l == 0 {
			col++
		}
		return ast.Using{Idents: idents}, col - 1
	case ';':
		return ast.Break{}, 1
	default:
		log.Fatal(inp[0:3])
	}
	return nil, 0
}

func ParseProgram(inp string) ([]ast.Statement, []string) {
	l_str := getOperand(inp)
	l, _ := strconv.Atoi(l_str)
	stmts := make([]ast.Statement, 0)
	col := 1 + len(l_str)
	for i := 0; i < l; i++ {
		stmt, c := ParseStatement(inp[col:])
		stmts = append(stmts, stmt)
		col += c + 1
	}
	l_str = getOperand(inp[col:])
	l, _ = strconv.Atoi(l_str)
	col += len(l_str) + 1
	types := make([]string, 0)
	for i := 0; i < l; i++ {
		typ := getType(inp[col:])
		types = append(types, typ)
		col += len(typ) + 1
	}
	return stmts, types
}

func getOperand(inp string) string {
	i := 0
	for ; len(inp) != i && inp[i] != ' '; i++ {
	}
	return inp[:i]
}

func getType(inp string) string {
	i := 0
	for ; len(inp) != i && inp[i] != '\n'; i++ {
	}
	return inp[:i]
}
