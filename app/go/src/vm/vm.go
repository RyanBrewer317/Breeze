package vm

import (
	"fmt"
	"log"
	"math"
	"reflect"

	"breeze.ryanbrewer.page/ast"
	"breeze.ryanbrewer.page/code"
	"breeze.ryanbrewer.page/compiler"
)

const StackSize = 2048
const GlobalsSize = 65536
const MaxFrames = 1024

var True = &ast.Bool{Value: true}
var False = &ast.Bool{Value: false}
var Null = &ast.Null{}

type VM struct {
	constants []ast.Expression

	stack []ast.Expression
	sp    int // Always points to the next value. Top of stack is stack[sp-1]

	globals []ast.Expression

	frames      []*Frame
	framesIndex int

	typeList []string
}

func New(bytecode *compiler.Bytecode, typeList []string) *VM {
	mainFn := &ast.CompiledFunction{Instructions: bytecode.Instructions}
	mainClosure := &ast.Closure{Fn: mainFn}
	mainFrame := NewFrame(mainClosure, 0)
	frames := make([]*Frame, MaxFrames)
	frames[0] = mainFrame
	return &VM{
		constants: bytecode.Constants,

		stack: make([]ast.Expression, StackSize),
		sp:    0,

		globals: make([]ast.Expression, GlobalsSize),

		frames:      frames,
		framesIndex: 1,

		typeList: typeList,
	}
}

func (vm *VM) StackTop() ast.Expression {
	if vm.sp == 0 {
		return nil
	}
	return vm.stack[vm.sp-1]
}

func (vm *VM) Run() error {
	// fmt.Println(vm.currentFrame().Instructions().String())
	var ip int
	var ins code.Instructions
	var op code.Opcode
	for vm.currentFrame().ip < len(vm.currentFrame().Instructions())-1 || vm.currentFrame() != vm.frames[0] {
		if !(vm.currentFrame().ip < len(vm.currentFrame().Instructions())-1) {
			vm.popFrame()
			continue
		}
		vm.currentFrame().ip++
		ip = vm.currentFrame().ip
		ins = vm.currentFrame().Instructions()
		op = code.Opcode(ins[ip])
		// def, err := code.Lookup(byte(op))
		// if err != nil {
		// 	return err
		// }
		// fmt.Println(def.Name)
		switch op {
		case code.OpConstant:
			constIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2

			err := vm.push(vm.constants[constIndex])
			if err != nil {
				return err
			}

		case code.OpAdd, code.OpSub, code.OpMul, code.OpDiv, code.OpExp, code.OpAnd, code.OpImplies, code.OpOr:
			if err := vm.executeBinaryOperation(op); err != nil {
				return err
			}

		case code.OpPop:
			vm.pop()

		case code.OpTrue:
			err := vm.push(True)
			if err != nil {
				return err
			}

		case code.OpFalse:
			err := vm.push(False)
			if err != nil {
				return err
			}

		case code.OpNull:
			err := vm.push(Null)
			if err != nil {
				return err
			}

		case code.OpEq, code.OpNEq, code.OpGT, code.OpGE:
			err := vm.executeComparison(op)
			if err != nil {
				return err
			}

		case code.OpNot:
			operand := vm.pop()
			switch operand {
			case True:
				vm.push(False)
			case False:
				vm.push(True)
			default:
				vm.push(False)
			}

		case code.OpNeg:
			operand := vm.pop()
			if operand.Type([]string{}) == "Int" {
				value := operand.(*ast.Int).Value
				vm.push(&ast.Int{Value: -value})
			} else if operand.Type([]string{}) == "Float" {
				value := operand.(*ast.Float).Value
				vm.push(&ast.Float{Value: -value})
			}

		case code.OpArrayType:
			operand := vm.pop()
			vm.push(&ast.ArrayType{InnerType: operand.(ast.Type)})

		case code.OpJump:
			pos := int(code.ReadUint16(ins[ip+1:]))
			vm.currentFrame().ip = pos - 1

		case code.OpJumpNotTruthy:
			pos := int(code.ReadUint16(ins[ip+1:]))
			vm.currentFrame().ip += 2
			condition := vm.pop()
			if !isTruthy(condition) {
				vm.currentFrame().ip = pos - 1
			}

		case code.OpSetGlobal:
			globalIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2
			vm.globals[globalIndex] = vm.pop()

		case code.OpGetGlobal:
			globalIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2
			err := vm.push(vm.globals[globalIndex])
			if err != nil {
				return err
			}

		case code.OpDotAssign:
			val := vm.pop()
			fieldIdent := vm.pop().(ast.Ident)
			field := fieldIdent.Name
			obj := vm.pop().(*ast.Struct)
			i := 0
			for ; i < len(obj.Names); i++ {
				if obj.Names[i] == field {
					break
				}
			}
			obj.Values[i] = val

		case code.OpIndexAssign:
			value := vm.pop()
			index := vm.pop().(*ast.Int)
			array := vm.pop().(*ast.Array)
			array.Exprs[index.Value] = value

		case code.OpArray:
			numElements := int(code.ReadUint16(ins[ip+1:]))
			typeInt := int(code.ReadUint16(ins[ip+3:]))
			vm.currentFrame().ip += 4
			array := vm.buildArray(vm.sp-numElements, vm.sp, typeInt)
			vm.sp = vm.sp - numElements
			err := vm.push(array)
			if err != nil {
				return err
			}

		case code.OpIndex:
			index := vm.pop()
			left := vm.pop()
			err := vm.executeIndexExpression(left, index)
			if err != nil {
				return err
			}

		case code.OpCall:
			numArgs := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip += 1
			err := vm.executeCall(int(numArgs))
			if err != nil {
				return err
			}

		case code.OpReturnValue:
			returnValue := vm.pop()
			inBlock := vm.currentFrame().Block
			for inBlock {
				vm.popFrame()
				inBlock = vm.currentFrame().Block
			}
			frame := vm.popFrame()
			vm.sp = frame.basePointer - 1
			err := vm.push(returnValue)
			if err != nil {
				return err
			}

		case code.OpReturn:
			inBlock := vm.currentFrame().Block
			for inBlock {
				vm.popFrame()
				inBlock = vm.currentFrame().Block
			}
			frame := vm.popFrame()
			vm.sp = frame.basePointer - 1
			err := vm.push(Null)
			if err != nil {
				return err
			}

		case code.OpSetLocal:
			localIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip += 1
			frame := vm.currentFrame()
			vm.stack[frame.basePointer+int(localIndex)] = vm.pop()

		case code.OpGetLocal:
			localIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip += 1
			frame := vm.currentFrame()
			err := vm.push(vm.stack[frame.basePointer+int(localIndex)])
			if err != nil {
				return err
			}

		case code.OpGetBuiltin:
			builtinIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip += 1
			if len(ast.Builtins) > int(builtinIndex) {
				definition := ast.Builtins[builtinIndex]
				err := vm.push(definition.Builtin)
				if err != nil {
					return err
				}
			} else {
				definition := ast.BuiltinVars[int(builtinIndex)-len(ast.Builtins)]
				err := vm.push(definition.Val)
				if err != nil {
					return err
				}
			}

		case code.OpClosure:
			constIndex := code.ReadUint16(ins[ip+1:])
			numFree := code.ReadUint8(ins[ip+3:])
			vm.currentFrame().ip += 3
			err := vm.pushClosure(int(constIndex), int(numFree))
			if err != nil {
				return err
			}

		case code.OpGetFree:
			freeIndex := code.ReadUint8(ins[ip+1:])
			vm.currentFrame().ip += 1
			currentClosure := vm.currentFrame().c
			switch currentClosure := currentClosure.(type) {
			case *ast.Closure:
				err := vm.push(currentClosure.Free[freeIndex])
				if err != nil {
					return err
				}
			}

		case code.OpCurrentClosure:
			currentClosure := vm.currentFrame().c
			err := vm.push(currentClosure)
			if err != nil {
				return err
			}

		case code.OpSwitch:
			expr := vm.pop()
			dereferencedExpr := reflect.ValueOf(expr).Elem().Interface().(ast.Expression)
			constIndex := code.ReadUint16(ins[ip+1:])
			jumpTable := vm.constants[constIndex]
			switch jumpTable := jumpTable.(type) {
			case *ast.JumpTable:
				pos, ok := jumpTable.Table[dereferencedExpr]
				if ok {
					vm.currentFrame().ip = pos - 1
				} else {
					vm.currentFrame().ip = jumpTable.Default - 1
				}
			case *ast.TypeJumpTable:
				pos, ok := jumpTable.Table[dereferencedExpr.Type(vm.typeList)]
				if ok {
					vm.currentFrame().ip = pos - 1
				} else {
					vm.currentFrame().ip = jumpTable.Default - 1
				}
			}

		case code.OpStruct:
			constIndex := code.ReadUint16(ins[ip+1:])
			vm.currentFrame().ip += 2
			structPtr := vm.constants[constIndex].(*ast.Struct)
			newVals := make([]ast.Expression, len(structPtr.Values))
			for i := len(structPtr.Values) - 1; i > -1; i-- {
				newVals[i] = vm.pop()
			}
			vm.push(&ast.Struct{TypeInt: structPtr.TypeInt, Names: structPtr.Names, Values: newVals})

		case code.OpDot:
			nameIdent := vm.pop().(*ast.Ident)
			obj := vm.pop().(*ast.Struct)
			i := 0
			for ; i < len(obj.Names); i++ {
				if obj.Names[i] == nameIdent.Name {
					break
				}
			}
			val := obj.Values[i]
			vm.push(val)

		default:
			log.Fatal(op)

		}
	}

	return nil
}

func (vm *VM) pushClosure(constIndex int, numFree int) error {
	constant := vm.constants[constIndex]
	function, ok := constant.(*ast.CompiledFunction)
	if !ok {
		return fmt.Errorf("not a function: %+v", constant)
	}
	free := make([]ast.Expression, numFree)
	for i := 0; i < numFree; i++ {
		free[i] = vm.stack[vm.sp-numFree+i]
	}
	vm.sp = vm.sp - numFree
	closure := &ast.Closure{Fn: function, Free: free}
	return vm.push(closure)
}

func (vm *VM) executeCall(numArgs int) error {
	callee := vm.stack[vm.sp-1-numArgs]
	switch callee := callee.(type) {
	case *ast.Closure:
		return vm.callClosure(callee, numArgs)
	case *ast.Builtin:
		return vm.callBuiltin(callee, numArgs)
	default:
		return fmt.Errorf("calling non-function and non-built-in %#v", callee)
	}
}

func (vm *VM) callClosure(cl *ast.Closure, numArgs int) error {
	if numArgs != cl.Fn.NumParameters {
		return fmt.Errorf("wrong number of arguments: want=%d, got=%d",
			cl.Fn.NumParameters, numArgs)
	}
	frame := NewFrame(cl, vm.sp-numArgs)
	vm.pushFrame(frame)
	vm.sp = frame.basePointer + cl.Fn.NumLocals
	return nil
}

func (vm *VM) callBuiltin(builtin *ast.Builtin, numArgs int) error {
	args := vm.stack[vm.sp-numArgs : vm.sp]
	result := builtin.Func(args...)
	vm.sp = vm.sp - numArgs - 1
	if result != nil {
		vm.push(result)
	} else {
		vm.push(Null)
	}
	return nil
}

func (vm *VM) currentFrame() *Frame {
	return vm.frames[vm.framesIndex-1]
}

func (vm *VM) pushFrame(f *Frame) {
	vm.frames[vm.framesIndex] = f
	vm.framesIndex++
}

func (vm *VM) popFrame() *Frame {
	vm.framesIndex--
	return vm.frames[vm.framesIndex]
}

func (vm *VM) executeIndexExpression(left, index ast.Expression) error {
	if _, ok := left.(*ast.Array); ok {
		return vm.executeArrayIndex(left, index)
	}
	log.Fatal("index on non-array")
	return nil
}

func (vm *VM) executeArrayIndex(array, index ast.Expression) error {
	arrayObject := array.(*ast.Array)
	i := index.(*ast.Int).Value
	max := int(len(arrayObject.Exprs) - 1)
	if i < 0 || i > max {
		return vm.push(Null)
	}
	return vm.push(arrayObject.Exprs[i])
}

func (vm *VM) buildArray(startIndex, endIndex int, typeInt int) ast.Expression {
	elements := make([]ast.Expression, endIndex-startIndex)
	for i := startIndex; i < endIndex; i++ {
		elements[i-startIndex] = vm.stack[i]
	}
	return &ast.Array{Exprs: elements, TypeInt: typeInt}
}

func isTruthy(obj ast.Expression) bool {
	switch obj := obj.(type) {
	case *ast.Bool:
		return obj.Value
	case *ast.Null:
		return false
	default:
		return true
	}
}

func (vm *VM) executeComparison(op code.Opcode) error {
	right := vm.pop()
	left := vm.pop()
	rightType := right.Type(vm.typeList)
	leftType := left.Type(vm.typeList)
	if leftType == "Int" && rightType == "Int" {
		return vm.executeIntegerComparison(op, left, right)
	} else if (leftType == "Int" || leftType == "Float") && (rightType == "Int" || rightType == "Float") {
		return vm.executeFloatComparison(op, left, right)
	} else if (leftType == "Char" || leftType == "Int") && (rightType == "Char" || rightType == "Int") {
		return vm.executeCharComparison(op, left, right)
	}
	switch op {
	case code.OpEq:
		return vm.push(nativeBoolToBooleanObject(right == left))
	case code.OpNEq:
		return vm.push(nativeBoolToBooleanObject(right != left))
	default:
		return fmt.Errorf("unknown operator: %d (%s %s)",
			op, leftType, rightType)
	}
}

func (vm *VM) executeCharComparison(op code.Opcode, left, right ast.Expression) error {
	var leftValue int
	var rightValue int
	switch left := left.(type) {
	case *ast.Int:
		leftValue = left.Value
	case *ast.Char:
		leftValue = int(left.Value)
	}
	switch right := right.(type) {
	case *ast.Int:
		rightValue = right.Value
	case *ast.Char:
		rightValue = int(right.Value)
	}
	switch op {
	case code.OpEq:
		return vm.push(nativeBoolToBooleanObject(leftValue == rightValue))
	case code.OpNEq:
		return vm.push(nativeBoolToBooleanObject(leftValue != rightValue))
	case code.OpGT:
		return vm.push(nativeBoolToBooleanObject(leftValue > rightValue))
	case code.OpGE:
		return vm.push(nativeBoolToBooleanObject(leftValue >= rightValue))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

func (vm *VM) executeIntegerComparison(op code.Opcode, left, right ast.Expression) error {
	leftValue := left.(*ast.Int).Value
	rightValue := right.(*ast.Int).Value
	switch op {
	case code.OpEq:
		return vm.push(nativeBoolToBooleanObject(leftValue == rightValue))
	case code.OpNEq:
		return vm.push(nativeBoolToBooleanObject(leftValue != rightValue))
	case code.OpGT:
		return vm.push(nativeBoolToBooleanObject(leftValue > rightValue))
	case code.OpGE:
		return vm.push(nativeBoolToBooleanObject(leftValue >= rightValue))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

func (vm *VM) executeFloatComparison(op code.Opcode, left, right ast.Expression) error {
	var leftValue float64
	var rightValue float64
	switch left := left.(type) {
	case *ast.Int:
		leftValue = float64(left.Value)
	case *ast.Float:
		leftValue = left.Value
	}
	switch right := right.(type) {
	case *ast.Int:
		rightValue = float64(right.Value)
	case *ast.Float:
		rightValue = right.Value
	}
	switch op {
	case code.OpEq:
		return vm.push(nativeBoolToBooleanObject(leftValue == rightValue))
	case code.OpNEq:
		return vm.push(nativeBoolToBooleanObject(leftValue != rightValue))
	case code.OpGT:
		return vm.push(nativeBoolToBooleanObject(leftValue > rightValue))
	case code.OpGE:
		return vm.push(nativeBoolToBooleanObject(leftValue >= rightValue))
	default:
		return fmt.Errorf("unknown operator: %d", op)
	}
}

func (vm *VM) executeBinaryFloatOperation(op code.Opcode, left, right ast.Expression) error {
	var leftValue float64
	var rightValue float64
	switch left := left.(type) {
	case *ast.Int:
		leftValue = float64(left.Value)
	case *ast.Float:
		leftValue = left.Value
	}
	switch right := right.(type) {
	case *ast.Int:
		rightValue = float64(right.Value)
	case *ast.Float:
		rightValue = right.Value
	}
	var result float64
	switch op {
	case code.OpAdd:
		result = leftValue + rightValue
	case code.OpSub:
		result = leftValue - rightValue
	case code.OpMul:
		result = leftValue * rightValue
	case code.OpDiv:
		result = leftValue / rightValue
	case code.OpExp:
		result = math.Pow(leftValue, rightValue)
	default:
		return fmt.Errorf("unknown float operator: %d", op)
	}
	return vm.push(&ast.Float{Value: result})
}

func nativeBoolToBooleanObject(input bool) *ast.Bool {
	if input {
		return True
	}
	return False
}

func (vm *VM) executeBinaryOperation(op code.Opcode) error {
	right := vm.pop()
	left := vm.pop()
	rightType := right.Type(vm.typeList)
	leftType := left.Type(vm.typeList)
	if rightType == "Int" && leftType == "Int" {
		return vm.executeBinaryIntegerOperation(op, left, right)
	} else if (leftType == "Int" || leftType == "Float") && (rightType == "Int" || rightType == "Float") {
		return vm.executeBinaryFloatOperation(op, left, right)
	} else if rightType == "Bool" && leftType == "Bool" {
		return vm.executeBinaryBooleanOperation(op, left, right)
	} else if leftType[:2] == "[]" && rightType[:2] == "[]" {
		return vm.executeBinaryArrayOperation(op, left, right)
	} else if leftType == "Type" && rightType == "Type" {
		return vm.executeBinaryTypeOperation(op, left.(ast.Type), right.(ast.Type))
	}
	return fmt.Errorf("unsupported types for binary operation: %s %s",
		leftType, rightType)
}

func (vm *VM) executeBinaryTypeOperation(op code.Opcode, left ast.Type, right ast.Type) error {
	switch op {
	case code.OpImplies:
		return vm.push(&ast.FunctionType{ArgType: left, RetType: right})
	case code.OpAdd:
		return vm.push(&ast.SumType{Left: left, Right: right})
	case code.OpMul:
		return vm.push(&ast.ProductType{Left: left, Right: right})
	default:
		return fmt.Errorf("unknown operator %d", op)
	}
}

func (vm *VM) executeBinaryBooleanOperation(op code.Opcode, left ast.Expression, right ast.Expression) error {
	leftValue := left.(*ast.Bool).Value
	rightValue := right.(*ast.Bool).Value
	var result bool
	switch op {
	case code.OpAnd:
		result = leftValue && rightValue
	case code.OpOr:
		result = leftValue || rightValue
	case code.OpImplies:
		result = !leftValue || rightValue
	default:
		return fmt.Errorf("unknown boolean operator: %d", op)
	}
	return vm.push(&ast.Bool{Value: result})
}

func (vm *VM) executeBinaryArrayOperation(op code.Opcode, left, right ast.Expression) error {
	leftValue := left.(*ast.Array).Exprs
	rightValue := right.(*ast.Array).Exprs
	return vm.push(&ast.Array{Exprs: append(leftValue, rightValue...)})
}

func (vm *VM) executeBinaryIntegerOperation(op code.Opcode, left ast.Expression, right ast.Expression) error {
	leftValue := left.(*ast.Int).Value
	rightValue := right.(*ast.Int).Value
	var result int
	switch op {
	case code.OpAdd:
		result = leftValue + rightValue
	case code.OpSub:
		result = leftValue - rightValue
	case code.OpMul:
		result = leftValue * rightValue
	case code.OpDiv:
		result = leftValue / rightValue
	case code.OpExp:
		result = int(math.Pow(float64(leftValue), float64(rightValue)))
	default:
		return fmt.Errorf("unknown integer operator: %d", op)
	}
	return vm.push(&ast.Int{Value: result})
}

func (vm *VM) push(o ast.Expression) error {
	if vm.sp >= StackSize {
		return fmt.Errorf("stack overflow")
	}

	vm.stack[vm.sp] = o
	vm.sp++

	return nil
}

func (vm *VM) pop() ast.Expression {
	o := vm.stack[vm.sp-1]
	vm.sp--
	return o
}

func (vm *VM) LastPoppedStackElem() ast.Expression {
	return vm.stack[vm.sp]
}
