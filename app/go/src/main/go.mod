module breeze.ryanbrewer.page/main

go 1.18

replace breeze.ryanbrewer.page/parser => ../parser

replace breeze.ryanbrewer.page/ast => ../ast

replace breeze.ryanbrewer.page/compiler => ../compiler

replace breeze.ryanbrewer.page/code => ../code

replace breeze.ryanbrewer.page/vm => ../vm

require (
	breeze.ryanbrewer.page/compiler v0.0.0-00010101000000-000000000000
	breeze.ryanbrewer.page/parser v0.0.0-00010101000000-000000000000
	breeze.ryanbrewer.page/vm v0.0.0-00010101000000-000000000000
)

require (
	breeze.ryanbrewer.page/ast v0.0.0-00010101000000-000000000000 // direct
	breeze.ryanbrewer.page/code v0.0.0-00010101000000-000000000000 // indirect
)
