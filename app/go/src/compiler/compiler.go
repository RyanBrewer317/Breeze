package compiler

import (
	"fmt"

	"breeze.ryanbrewer.page/ast"
	"breeze.ryanbrewer.page/code"
)

type CompilationScope struct {
	instructions        code.Instructions
	lastInstruction     EmittedInstruction
	previousInstruction EmittedInstruction
}

type Compiler struct {
	constants   []ast.Expression
	symbolTable *SymbolTable
	scopes      []CompilationScope
	scopeIndex  int
	typeList    []string
}

func (c *Compiler) currentInstructions() code.Instructions {
	return c.scopes[c.scopeIndex].instructions
}

func New(typeList []string) *Compiler {
	mainScope := CompilationScope{
		instructions:        code.Instructions{},
		lastInstruction:     EmittedInstruction{},
		previousInstruction: EmittedInstruction{},
	}
	symbolTable := NewSymbolTable()
	for i, v := range ast.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}
	next := len(symbolTable.store)
	for i, v := range ast.BuiltinVars {
		symbolTable.DefineBuiltin(next+i, v.Name)
	}
	return &Compiler{
		constants:   []ast.Expression{},
		symbolTable: symbolTable,
		scopes:      []CompilationScope{mainScope},
		scopeIndex:  0,
		typeList:    typeList,
	}
}

func (c *Compiler) loadSymbol(s Symbol) {
	switch s.Scope {
	case GlobalScope:
		c.emit(code.OpGetGlobal, s.Index)
	case LocalScope:
		c.emit(code.OpGetLocal, s.Index)
	case BuiltinScope:
		c.emit(code.OpGetBuiltin, s.Index)
	case FreeScope:
		c.emit(code.OpGetFree, s.Index)
	case FunctionScope:
		c.emit(code.OpCurrentClosure)
	}
}

func (c *Compiler) CompileProgram(stmts []ast.Statement) error {
	breaks := make([]int, 0)
	var err error
	for _, s := range stmts {
		breaks, err = c.CompileStatement(s)
		if err != nil {
			return err
		}
	}
	breakJumpPos := len(c.currentInstructions())
	for i := 0; i < len(breaks); i++ {
		c.changeOperand(breaks[i], breakJumpPos)
	}
	return nil
}

func (c *Compiler) CompileStatement(stmt ast.Statement) ([]int, error) {
	switch stmt := stmt.(type) {
	case ast.Assignment:
		symbol, exists := c.symbolTable.store[stmt.Name]
		if !exists {
			symbol = c.symbolTable.Define(stmt.Name)
		}
		err := c.CompileExpression(stmt.Value)
		if err != nil {
			return nil, err
		}
		if symbol.Scope == GlobalScope {
			c.emit(code.OpSetGlobal, symbol.Index)
		} else {
			c.emit(code.OpSetLocal, symbol.Index)
		}
		return []int{}, nil
	case ast.DotAssignment:
		err := c.CompileExpression(stmt.Struct)
		if err != nil {
			return nil, err
		}
		c.emit(code.OpConstant, c.addConstant(ast.Ident{Name: stmt.Field}))
		err = c.CompileExpression(stmt.Value)
		if err != nil {
			return nil, err
		}
		c.emit(code.OpDotAssign)
		return []int{}, nil
	case ast.IndexAssignment:
		err := c.CompileExpression(stmt.Array)
		if err != nil {
			return nil, err
		}
		err = c.CompileExpression(stmt.Index)
		if err != nil {
			return nil, err
		}
		err = c.CompileExpression(stmt.Value)
		if err != nil {
			return nil, err
		}
	case ast.FunctionDeclaration:
		c.enterScope()
		c.symbolTable.DefineFunctionName(stmt.Name)
		for _, p := range stmt.Args {
			c.symbolTable.Define(p.Name)
		}
		breaks := make([]int, 0)
		for _, s := range stmt.Block.Statements {
			stmtBreaks, err := c.CompileStatement(s)
			if err != nil {
				return nil, err
			}
			breaks = append(breaks, stmtBreaks...)
		}
		if !c.lastInstructionIs(code.OpReturnValue) {
			c.emit(code.OpReturn)
		}
		breakJumpPos := len(c.currentInstructions())
		for i := 0; i < len(breaks); i++ {
			c.changeOperand(breaks[i], breakJumpPos)
		}
		freeSymbols := c.symbolTable.FreeSymbols
		numLocals := c.symbolTable.numDefinitions
		instructions := c.leaveScope()
		for _, s := range freeSymbols {
			c.loadSymbol(s)
		}
		compiledFn := &ast.CompiledFunction{
			Instructions:  instructions,
			NumLocals:     numLocals,
			NumParameters: len(stmt.Args)}
		fnIndex := c.addConstant(compiledFn)
		c.emit(code.OpClosure, fnIndex, len(freeSymbols))
		symbol := c.symbolTable.Define(stmt.Name)
		if symbol.Scope == GlobalScope {
			c.emit(code.OpSetGlobal, symbol.Index)
		} else {
			c.emit(code.OpSetLocal, symbol.Index)
		}
		return []int{}, nil
	case ast.Return:
		err := c.CompileExpression(stmt.Expr)
		if err != nil {
			return nil, err
		}
		c.emit(code.OpReturnValue)
		return []int{}, nil
	case ast.Using:
		for i := 0; i < len(stmt.Idents); i++ {
			symbol, _ := c.symbolTable.Resolve(stmt.Idents[i])
			c.symbolTable.Add(symbol)
		}
		return []int{}, nil
	case ast.Execution:
		err := c.CompileExpression(stmt.Expr)
		if err != nil {
			return nil, err
		}
		c.emit(code.OpPop)
		return []int{}, nil
	case ast.If:
		err := c.CompileExpression(stmt.Cond)
		if err != nil {
			return nil, err
		}
		// Emit an `OpJumpNotTruthy` with a bogus value
		jumpNotTruthyPos := c.emit(code.OpJumpNotTruthy, 9999)
		var breaks []int
		breaks, err = c.CompileStatement(stmt.Block)
		if err != nil {
			return nil, err
		}
		afterConsequencePos := len(c.currentInstructions())
		c.changeOperand(jumpNotTruthyPos, afterConsequencePos)
		return breaks, nil
	case ast.IfElse:
		err := c.CompileExpression(stmt.Cond)
		if err != nil {
			return nil, err
		}
		// Emit an `OpJumpNotTruthy` with a bogus value
		jumpNotTruthyPos := c.emit(code.OpJumpNotTruthy, 9999)
		var breaks []int
		breaks, err = c.CompileStatement(stmt.Cons)
		if err != nil {
			return nil, err
		}
		// Emit an `OpJump` with a bogus value
		jumpPos := c.emit(code.OpJump, 9999)
		afterConsequencePos := len(c.currentInstructions())
		c.changeOperand(jumpNotTruthyPos, afterConsequencePos)
		var altBreaks []int
		altBreaks, err = c.CompileStatement(stmt.Alt)
		breaks = append(breaks, altBreaks...)
		if err != nil {
			return nil, err
		}
		afterAlternativePos := len(c.currentInstructions())
		c.changeOperand(jumpPos, afterAlternativePos)
		return breaks, nil
	case ast.Switch:
		err := c.CompileExpression(stmt.Expr)
		if err != nil {
			return nil, err
		}
		switchOp := c.emit(code.OpSwitch, 9999)
		jumpTable := make(map[ast.Expression]int)
		breaks := make([]int, 0)
		for val, block := range stmt.Cases {
			pos := len(c.currentInstructions())
			stmtBreaks, err := c.CompileStatement(block)
			if err != nil {
				return nil, err
			}
			breaks = append(breaks, stmtBreaks...)
			jumpTable[val] = pos
		}
		defaultPos := len(c.currentInstructions())
		var stmtBreaks []int
		stmtBreaks, err = c.CompileStatement(stmt.Default)
		if err != nil {
			return nil, err
		}
		afterSwitchPos := len(c.currentInstructions())
		breaks = append(breaks, stmtBreaks...)
		for i := 0; i < len(breaks); i++ {
			c.changeOperand(breaks[i], afterSwitchPos)
		}
		c.changeOperand(switchOp, c.addConstant(&ast.JumpTable{Table: jumpTable, Default: defaultPos}))
		return []int{}, nil
	case ast.TypeSwitch:
		err := c.CompileExpression(stmt.Expr)
		if err != nil {
			return nil, err
		}
		switchOp := c.emit(code.OpSwitch, 9999)
		jumpTable := make(map[string]int)
		breaks := make([]int, 0)
		for val, block := range stmt.Cases {
			pos := len(c.currentInstructions())
			stmtBreaks, err := c.CompileStatement(block)
			if err != nil {
				return nil, err
			}
			breaks = append(breaks, stmtBreaks...)
			breaks = append(breaks, c.emit(code.OpJump, 9999)) // typeswitch breaks after each case, unlike switch
			jumpTable[val.Type(c.typeList)] = pos
		}
		defaultPos := len(c.currentInstructions())
		var stmtBreaks []int
		stmtBreaks, err = c.CompileStatement(stmt.Default)
		if err != nil {
			return nil, err
		}
		afterSwitchPos := len(c.currentInstructions())
		breaks = append(breaks, stmtBreaks...)
		for i := 0; i < len(breaks); i++ {
			c.changeOperand(breaks[i], afterSwitchPos)
		}
		c.changeOperand(switchOp, c.addConstant(&ast.TypeJumpTable{Table: jumpTable, Default: defaultPos}))
		return []int{}, nil
	case ast.While:
		startPos := len(c.currentInstructions())
		err := c.CompileExpression(stmt.Cond)
		if err != nil {
			return nil, err
		}
		jumpPos := c.emit(code.OpJumpNotTruthy, 9999)
		var breaks []int
		breaks, err = c.CompileStatement(stmt.Block)
		if err != nil {
			return nil, err
		}
		c.emit(code.OpJump, startPos)
		afterBlock := len(c.currentInstructions())
		c.changeOperand(jumpPos, afterBlock)
		for i := 0; i < len(breaks); i++ {
			c.changeOperand(breaks[i], afterBlock)
		}
		return []int{}, nil
	case ast.For:
		breaks := make([]int, 0)
		stmtBreaks, err := c.CompileStatement(stmt.Init)
		if err != nil {
			return nil, err
		}
		breaks = append(breaks, stmtBreaks...)
		startPos := len(c.currentInstructions())
		err = c.CompileExpression(stmt.Cond)
		if err != nil {
			return nil, err
		}
		jumpPos := c.emit(code.OpJumpNotTruthy, 9999)
		switch block := stmt.Block.(type) {
		case ast.Block:
			for _, s := range block.Statements {
				stmtBreaks, err := c.CompileStatement(s)
				if err != nil {
					return nil, err
				}
				breaks = append(breaks, stmtBreaks...)
			}
		default:
			stmtBreaks, err = c.CompileStatement(block)
			if err != nil {
				return nil, err
			}
			breaks = append(breaks, stmtBreaks...)
		}
		stmtBreaks, err = c.CompileStatement(stmt.Iter)
		if err != nil {
			return nil, err
		}
		breaks = append(breaks, stmtBreaks...)
		c.emit(code.OpJump, startPos)
		afterBlock := len(c.currentInstructions())
		c.changeOperand(jumpPos, afterBlock)
		for i := 0; i < len(breaks); i++ {
			c.changeOperand(breaks[i], afterBlock)
		}
		return []int{}, nil
	case ast.Block:
		breaks := make([]int, 0)
		for _, s := range stmt.Statements {
			stmtBreaks, err := c.CompileStatement(s)
			if err != nil {
				return nil, err
			}
			breaks = append(breaks, stmtBreaks...)
		}
		return breaks, nil
	case ast.Break:
		pos := c.emit(code.OpJump, 9999)
		return []int{pos}, nil
	}
	return nil, nil
}

func (c *Compiler) CompileExpression(expr ast.Expression) error {
	switch expr := expr.(type) {
	case ast.Lambda:
		c.enterScope()
		for _, p := range expr.Args {
			c.symbolTable.Define(p.Name)
		}
		breaks := make([]int, 0)
		for _, s := range expr.Block.Statements {
			stmtBreaks, err := c.CompileStatement(s)
			if err != nil {
				return err
			}
			breaks = append(breaks, stmtBreaks...)
		}
		if !c.lastInstructionIs(code.OpReturnValue) {
			c.emit(code.OpReturn)
		}
		breakJumpPos := len(c.currentInstructions())
		for i := 0; i < len(breaks); i++ {
			c.changeOperand(breaks[i], breakJumpPos)
		}
		freeSymbols := c.symbolTable.FreeSymbols
		numLocals := c.symbolTable.numDefinitions
		instructions := c.leaveScope()
		for _, s := range freeSymbols {
			c.loadSymbol(s)
		}
		compiledFn := &ast.CompiledFunction{
			Instructions:  instructions,
			NumLocals:     numLocals,
			NumParameters: len(expr.Args)}
		fnIndex := c.addConstant(compiledFn)
		c.emit(code.OpClosure, fnIndex, len(freeSymbols))
	case ast.Infix:
		if expr.Op == "<" || expr.Op == "<=" {
			err := c.CompileExpression(expr.Right)
			if err != nil {
				return err
			}
			err = c.CompileExpression(expr.Left)
			if err != nil {
				return err
			}
			if expr.Op == "<" {
				c.emit(code.OpGT)
			} else {
				c.emit(code.OpGE)
			}
			return nil
		}
		if err := c.CompileExpression(expr.Left); err != nil {
			return err
		}
		if err := c.CompileExpression(expr.Right); err != nil {
			return err
		}
		switch expr.Op {
		case "+":
			c.emit(code.OpAdd)
		case "-":
			c.emit(code.OpSub)
		case "*":
			c.emit(code.OpMul)
		case "/":
			c.emit(code.OpDiv)
		case "^":
			c.emit(code.OpExp)
		case "&&":
			c.emit(code.OpAnd)
		case "||":
			c.emit(code.OpOr)
		case "=>":
			c.emit(code.OpImplies)
		case ">":
			c.emit(code.OpGT)
		case ">=":
			c.emit(code.OpGE)
		case "==":
			c.emit(code.OpEq)
		case "!=":
			c.emit(code.OpNEq)
		default:
			return fmt.Errorf("unknown operator %s", expr.Op)
		}
	case ast.Prefix:
		err := c.CompileExpression(expr.Right)
		if err != nil {
			return err
		}
		switch expr.Op {
		case "!":
			c.emit(code.OpNot)
		case "-":
			c.emit(code.OpNeg)
		case "[]":
			c.emit(code.OpArrayType)
		default:
			return fmt.Errorf("unknown operator %s", expr.Op)
		}
	case ast.Ternary:
		err := c.CompileExpression(expr.Cond)
		if err != nil {
			return err
		}
		// Emit an `OpJumpNotTruthy` with a bogus value
		jumpNotTruthyPos := c.emit(code.OpJumpNotTruthy, 9999)
		err = c.CompileExpression(expr.Cons)
		if err != nil {
			return err
		}
		// Emit an `OpJump` with a bogus value
		jumpPos := c.emit(code.OpJump, 9999)
		afterConsequencePos := len(c.currentInstructions())
		c.changeOperand(jumpNotTruthyPos, afterConsequencePos)
		err = c.CompileExpression(expr.Alt)
		if err != nil {
			return err
		}
		afterAlternativePos := len(c.currentInstructions())
		c.changeOperand(jumpPos, afterAlternativePos)
	case ast.Array:
		for _, el := range expr.Exprs {
			err := c.CompileExpression(el)
			if err != nil {
				return err
			}
		}
		c.emit(code.OpArray, len(expr.Exprs), expr.TypeInt)
	case ast.Index:
		err := c.CompileExpression(expr.Collection)
		if err != nil {
			return err
		}
		err = c.CompileExpression(expr.Value)
		if err != nil {
			return err
		}
		c.emit(code.OpIndex)
	case ast.Call:
		err := c.CompileExpression(expr.Func)
		if err != nil {
			return err
		}
		for _, a := range expr.Arg.Exprs {
			err := c.CompileExpression(a)
			if err != nil {
				return err
			}
		}
		c.emit(code.OpCall, len(expr.Arg.Exprs))
	case ast.Ident:
		symbol, ok := c.symbolTable.Resolve(expr.Name)
		if !ok {
			return fmt.Errorf("undefined variable %s", expr.Name)
		}
		c.loadSymbol(symbol)
	case ast.Int:
		c.emit(code.OpConstant, c.addConstant(&expr))
	case ast.Float:
		c.emit(code.OpConstant, c.addConstant(&expr))
	case ast.Char:
		c.emit(code.OpConstant, c.addConstant(&expr))
	case ast.Bool:
		if expr.Value {
			c.emit(code.OpTrue)
		} else {
			c.emit(code.OpFalse)
		}
	case ast.Null:
		c.emit(code.OpNull)
	case ast.StructType:
		c.emit(code.OpConstant, c.addConstant(&expr))
	case ast.InterfaceType:
		c.emit(code.OpConstant, c.addConstant(&expr))
	case ast.Struct:
		for _, v := range expr.Values {
			err := c.CompileExpression(v)
			if err != nil {
				return err
			}
		}
		c.emit(code.OpStruct, c.addConstant(&expr))
	case ast.Dot:
		err := c.CompileExpression(expr.Obj)
		if err != nil {
			return nil
		}
		c.emit(code.OpConstant, c.addConstant(&ast.Ident{Name: expr.FieldName, TypeInt: expr.TypeInt}))
		c.emit(code.OpDot)
	}
	return nil
}

func (c *Compiler) enterScope() {
	scope := CompilationScope{
		instructions:        code.Instructions{},
		lastInstruction:     EmittedInstruction{},
		previousInstruction: EmittedInstruction{},
	}
	c.scopes = append(c.scopes, scope)
	c.scopeIndex++
	c.symbolTable = NewEnclosedSymbolTable(c.symbolTable)
}

func (c *Compiler) leaveScope() code.Instructions {
	instructions := c.currentInstructions()
	c.scopes = c.scopes[:len(c.scopes)-1]
	c.scopeIndex--
	c.symbolTable = c.symbolTable.Outer
	return instructions
}

func (c *Compiler) lastInstructionIs(op code.Opcode) bool {
	if len(c.currentInstructions()) == 0 {
		return false
	}
	return c.scopes[c.scopeIndex].lastInstruction.Opcode == op
}

func (c *Compiler) replaceInstruction(pos int, newInstruction []byte) {
	ins := c.currentInstructions()
	for i := 0; i < len(newInstruction); i++ {
		ins[pos+i] = newInstruction[i]
	}
}

func (c *Compiler) changeOperand(opPos int, operand int) {
	op := code.Opcode(c.currentInstructions()[opPos])
	newInstruction := code.Make(op, operand)
	c.replaceInstruction(opPos, newInstruction)
}

func (c *Compiler) Bytecode() *Bytecode {
	return &Bytecode{
		Instructions: c.currentInstructions(),
		Constants:    c.constants,
	}
}

func (c *Compiler) addConstant(obj ast.Expression) int {
	c.constants = append(c.constants, obj)
	return len(c.constants) - 1
}

func (c *Compiler) emit(op code.Opcode, operands ...int) int {
	ins := code.Make(op, operands...)
	pos := c.addInstruction(ins)
	c.setLastInstruction(op, pos)
	return pos
}

func (c *Compiler) setLastInstruction(op code.Opcode, pos int) {
	previous := c.scopes[c.scopeIndex].lastInstruction
	last := EmittedInstruction{Opcode: op, Position: pos}
	c.scopes[c.scopeIndex].previousInstruction = previous
	c.scopes[c.scopeIndex].lastInstruction = last
}

func (c *Compiler) addInstruction(ins []byte) int {
	posNewInstruction := len(c.currentInstructions())
	updatedInstructions := append(c.currentInstructions(), ins...)
	c.scopes[c.scopeIndex].instructions = updatedInstructions
	return posNewInstruction
}

type Bytecode struct {
	Instructions code.Instructions
	Constants    []ast.Expression
}

type EmittedInstruction struct {
	Opcode   code.Opcode
	Position int
}
