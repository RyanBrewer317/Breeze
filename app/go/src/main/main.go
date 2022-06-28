package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"breeze.ryanbrewer.page/compiler"
	"breeze.ryanbrewer.page/parser"
	"breeze.ryanbrewer.page/vm"
)

func main() {
	fn := os.Args[1]
	buf, err := ioutil.ReadFile(fn)
	if err != nil {
		log.Fatal(err)
	}
	code := string(buf)
	stmts, typeList := parser.ParseProgram(code)
	comp := compiler.New(typeList)
	err = comp.CompileProgram(stmts)
	if err != nil {
		fmt.Printf("Woops! Compilation failed:\n %s\n", err)
	}

	machine := vm.New(comp.Bytecode(), typeList)
	err = machine.Run()
	if err != nil {
		fmt.Printf("Woops! Executing bytecode failed:\n %s\n", err)
	}
}
