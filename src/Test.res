module Test = {
  module Comp = Compiler.Compile 
  module Lang = Compiler.TinyLang4
  module Asm = Assembler.StackMachine

  let testCases = {
    open Lang 
    list{
    Let("a", Cst(2), 
      Letfn("cube", list{"x"},
        Letfn("square", list{"x"},
          Prim(Mul,list{Var("x"),Var("x")}),
          Prim(Mul,list{App("square",list{Var("x")}),Var("x")})),
        App("cube",list{Var("a")})))
    ,
    Letfn("fib", list{"n"},
      If(Prim(Leq, list{Var("n"), Cst(1)}),
        Cst(1),
        Prim(Add, list{
          App("fib", list{Prim(Add, list{Var("n"), Cst(-1)})}),
          App("fib", list{Prim(Add, list{Var("n"), Cst(-2)})})
          })),
      App("fib",list{Cst(5)}))
    }
  }

  let test = {
    let unitTest = (expr: Lang.expr) => {
      Js.log("---")
      Js.log(Lang.toString(expr))
      let evaluated = Lang.eval(expr)
      Js.log("Evaluated to : " ++ Js.Int.toString(evaluated))
      let asm = Comp.compile(expr)->Belt.List.toArray
      Js.log("Compiles to : ")
      Js.log(Asm.toString(asm))
      let byte = Asm.encode(asm)
      Js.log("Assembles to stack machine code: ")
      Js.log(byte)
    }
    testCases->Belt.List.forEach(unitTest)
  }
}