module TinyLang4 = {
  type prim = Add | Mul | Leq 
  type rec expr = 
    | Cst (int)
    | Var (string)
    | Let (string, expr, expr)
    | Letfn (string, list<string>, expr, expr)
    | App (string, list<expr>)
    | Prim (prim, list<expr>)
    | If (expr, expr, expr)

  type fun = (string, list<string>, expr)
  type prog = list<fun>

  let toString = (expr: expr) : string => {
    let argListString = (args: list<string>) : string => switch args {
    | list{} => "()"
    | list{a, ...res} => {
      "(" ++ List.fold_left((str, es)=>str++","++es,a,res) ++ ")"
    }
    }
    let rec humanReadable = (expr: expr) : string => switch expr {
    | Cst(i) => Js.Int.toString(i)
    | Var(s) => s 
    | Let (vname, def, app) => "let " ++ vname ++ " = {" ++ humanReadable(def)
      ++ "} in {" ++ humanReadable(app) ++ "}"
    | Letfn (fn, args, def, app) => "letfn " ++ fn ++ argListString(args) ++ " = {"
      ++ humanReadable(def) ++ "} in {" ++ humanReadable(app) ++ "}"
    | App (fn, args) => fn ++ argListString(args->Belt.List.map(humanReadable))
    | Prim (p, args) => {
      let primary = switch p {
      | Add => "+"
      | Mul => "*"
      | Leq => "<="
      | _ => assert false 
      }
      let primary_sub_str = (arg: expr) : string => switch arg {
        | Prim(_, _) => "(" ++ humanReadable(arg) ++ ")"
        | _ => humanReadable(arg)
      }
      if (Belt.List.length(args) == 2) {
        let arg1 = args->Belt.List.getExn(0)
        let arg2 = args->Belt.List.getExn(1)
        primary_sub_str(arg1) ++ " " ++ primary ++ " " ++ primary_sub_str(arg2) 
      }
      else {
        argListString(list{primary, ...args->Belt.List.map(humanReadable)})
      }
    }
    | If (cond, bTrue, bFalse) => "if " ++ humanReadable(cond) 
      ++ " then " ++ "{" ++ humanReadable(bTrue) ++ "}" 
      ++ " else " ++ "{" ++ humanReadable(bFalse) ++ "}"
    }
    humanReadable(expr)
  }

  type rec value = 
    | Vint (int)
    | Vclosure (env, list<string>, expr)
  and env = list<(string, value)>
  let vadd = (v1, v2) : value => switch (v1,v2) {
    | (Vint(i), Vint(j)) => Vint(i+j)
    | _ => assert false 
  }
  let vmul = (v1, v2) : value => switch (v1,v2) {
    | (Vint(i), Vint(j)) => Vint(i*j)
    | _ => assert false 
  }
  let vleq = (v1, v2) : value => switch (v1,v2) {
    | (Vint(i), Vint(j)) => Vint(i <= j ? 1 : 0)
    | _ => assert false 
  }
  let eval = (expr: expr): int => {
    let rec evalHelper = (expr:expr, env: env) : value => switch expr {
      | Cst(i) => Vint(i)
      | Prim(p, es) => {
        let v1 = evalHelper(es->Belt.List.getExn(0),env)
        let v2 = evalHelper(es->Belt.List.getExn(1),env)
        switch p {
          | Add => vadd(v1,v2)
          | Mul => vmul(v1,v2)
          | Leq => vleq(v1,v2)
          | _ => assert false 
        }
      }
      | Var(x) => switch Belt.List.getAssoc(env, x, (a,b)=>a==b) {
        | Some(v) => v 
        | _ => assert false  
      }
      | Let(vname, def, app) => evalHelper(app, list{(vname,evalHelper(def, env)), ...env})
      | Letfn(fn, args, body, app) => {
        let closure = Vclosure(env, args, body)
        evalHelper(app, list{(fn, closure), ...env})
      }
      | App(fn, args) => {
        let Vclosure(env_closure, args_closure, body) = switch Belt.List.getAssoc(env, fn, (a,b)=>a==b) {
          | Some(v) => v 
          | _ => assert false 
        }
        let params = args->Belt.List.map((e)=> evalHelper(e, env))
        let fun_env = Belt.List.concatMany([Belt.List.zip(args_closure, params), env])
        evalHelper(body, fun_env)
      }
      | If(cond, bTrue, bFalse) => {
        let Vint(c) = evalHelper(cond, env)
        if c == 0 {
          evalHelper(bFalse, env)
        } 
        else {
          evalHelper(bTrue, env)
        }
      }
    }
    let Vint(retval) = evalHelper(expr, list{})
    retval 
  }
}

module Flat = {
  type prim = TinyLang4.prim
  type rec expr =
    | Cst (int)
    | Var (string)
    | Let (string, expr, expr)
    | App (string, list<expr>)
    | Prim (prim, list<expr>)
    | If (expr, expr, expr)

  let toString = (expr: expr) : string => {
    let argListString = (args: list<string>) : string => switch args {
    | list{} => "()"
    | list{a, ...res} => {
      "(" ++ List.fold_left((str, es)=>str++","++es,a,res) ++ ")"
    }
    }
    let rec humanReadable = (expr: expr) : string => switch expr {
    | Cst(i) => Js.Int.toString(i)
    | Var(s) => s 
    | Let (vname, def, app) => "let " ++ vname ++ " = {" ++ humanReadable(def)
      ++ "} in {" ++ humanReadable(app) ++ "}"
    | App (fn, args) => fn ++ argListString(args->Belt.List.map(humanReadable))
    | Prim (p, args) => {
      let primary = switch p {
      | Add => "+"
      | Mul => "*"
      | Leq => "<="
      | _ => assert false 
      }
      let primary_sub_str = (arg: expr) : string => switch arg {
        | Prim(_, _) => "(" ++ humanReadable(arg) ++ ")"
        | _ => humanReadable(arg)
      }
      if (Belt.List.length(args) == 2) {
        let arg1 = args->Belt.List.getExn(0)
        let arg2 = args->Belt.List.getExn(1)
        primary_sub_str(arg1) ++ " " ++ primary ++ " " ++ primary_sub_str(arg2) 
      }
      else {
        argListString(list{primary, ...args->Belt.List.map(humanReadable)})
      }
    }
    | If (cond, bTrue, bFalse) => "if " ++ humanReadable(cond) 
      ++ " then " ++ "{" ++ humanReadable(bTrue) ++ "}" 
      ++ " else " ++ "{" ++ humanReadable(bFalse) ++ "}"
    }
    humanReadable(expr)
  }

} 

module Compile = {
  type expr = TinyLang4.expr 
  type instr = Assembler.StackMachine.instr

  type fun = (string, list<string>, Flat.expr)
  type prog = list<fun>

  let listToString = (args: list<string>) : string => switch args {
  | list{} => "[]"
  | list{a, ...res} => {
    "[" ++ List.fold_left((str, es)=>str++","++es,a,res) ++ "]"
  }
  } 
  let toString = (fun : fun) : string => {
    let (fn, args, body) = fun 
    "(" ++ fn ++ ", " ++ listToString(args) ++ ", " ++ Flat.toString(body) ++ ")"
  }
  let toString = (prog : prog) : string => {
    listToString(prog->Belt.List.map(toString))
  }


  // Auxiliary functions
  let rec remove_funs = (expr: expr): Flat.expr => switch expr {
    | Cst(i) => Flat.Cst(i)
    | Var(s) => Flat.Var(s)
    | Let(name, def, body) => Flat.Let(name, remove_funs(def), remove_funs(body))
    | Letfn(_, _, _, body) => remove_funs(body)
    | App(fname, args) => Flat.App(fname, args->Belt.List.map(remove_funs))
    | Prim(p, args) => Flat.Prim(p, args->Belt.List.map(remove_funs))
    | If(cond, bTrue, bFalse) => Flat.If(remove_funs(cond), remove_funs(bTrue), remove_funs(bFalse))
  }
  let rec collect_funs = (expr: expr): list<fun> => switch expr {
    | Cst(_) => list{}
    | Var(_) => list{}
    | Let(_, def, body) => Belt.List.concatMany([
      collect_funs(def),
      collect_funs(body)
    ])
    | Letfn(fname, args, def, app) => Belt.List.concatMany([
      list{(fname, args, remove_funs(def))},
      collect_funs(def),
      collect_funs(app)
    ])
    | App(_, args) => Belt.List.concatMany(args->Belt.List.map(collect_funs)->Belt.List.toArray)
    | Prim(_, args) => Belt.List.concatMany(args->Belt.List.map(collect_funs)->Belt.List.toArray)
    | If(cond, bTrue, bFalse) => Belt.List.concatMany([
      collect_funs(cond),
      collect_funs(bTrue),
      collect_funs(bFalse)
    ])
  }
  // Preprocessing
  let preprocess = (expr: expr): list<fun> => {
    let main = ("main", list{}, remove_funs(expr)) 
    let rest = collect_funs(expr)
    list{ main, ...rest }
  }


  type var = Local(string) | Temp //  Params and locals are treated uniformly
  type env = list<var>
  let get_index = (env: env, ele: string) : option<int> => {
    let rec index_getter = (env: env, ele: string, level: int) : option<int> => switch env {
    | list{} => None
    | list{a, ..._} if a == Local(ele) => Some(level)
    | list{_, ...b} => index_getter(b, ele, level+1)
    }
    index_getter(env, ele, 0)
  }
  let label_counter = ref(0)
  let get_new_label = () : string => {
    label_counter.contents = label_counter.contents + 1
    ".L" ++ Js.Int.toString(label_counter.contents)
  }


  // compile expression under a compile-time environment
  let rec compile_expr = (env:env, expr: Flat.expr) : list<instr> => switch expr {
  | Cst(i) => list{Assembler.StackMachine.Cst(i)}
  | Var(x) => switch get_index(env, x) {
    | Some(i) => list{Assembler.StackMachine.Var(i)}
    | _ => assert false 
  }
  | Let(x, def, app) => Belt.List.concatMany([
    compile_expr(env, def),
    compile_expr(list{Local(x), ...env}, app),
    list{Assembler.StackMachine.Swap, Assembler.StackMachine.Pop}
  ])
  | Prim(p, args) => {
    let primary = switch p {
      | Add => Assembler.StackMachine.Add 
      | Mul => Assembler.StackMachine.Mul
      | Leq => Assembler.StackMachine.Leq
      | _ => assert false 
    }
    let args_code = compile_exprs(env, args)
    Belt.List.concatMany([
      args_code,
      list{primary}
    ])
  }
  | App(fn, args) => {
    let n = Belt.List.length(args)
    let args_code = compile_exprs(env, args)
    Belt.List.concatMany([
      args_code,
      list{Assembler.StackMachine.Call(fn, n)}
    ])
  }
  | If(cond, bTrue, bFalse) => {
    let bFalse_label = get_new_label()
    let bEnd_label = get_new_label()
    Belt.List.concatMany([
      compile_expr(env, cond),
      list{Assembler.StackMachine.IfZero(bFalse_label)},
      compile_expr(env, bTrue),
      list{Assembler.StackMachine.Goto(bEnd_label)},
      list{Assembler.StackMachine.Label(bFalse_label)},
      compile_expr(env, bFalse),
      list{Assembler.StackMachine.Label(bEnd_label)}
    ])
  }
  }
  and compile_exprs = (env: env, exprs: list<Flat.expr>) : list<instr> => switch exprs {
    | list{} => list{}
    | list{expr, ...res} => Belt.List.concatMany([
      compile_expr(env, expr),
      compile_exprs(list{Temp, ...env}, res)
    ])
  }


  // compile functions
  let compile_fun = ((name, args, body): fun): list<instr> => {
    let n = Belt.List.length(args)
    let env = Belt.List.reverse(args->Belt.List.map((a) => Local(a)))
    Belt.List.concatMany([
      list{Assembler.StackMachine.Label(name)},
      compile_expr(env, body),
      list{Assembler.StackMachine.Ret(n)}
    ])
  }

  // compile the whole program
  let compile = (expr: expr) : list<instr> => { 
    let funs = preprocess(expr)
    let funs_code = Belt.List.concatMany(funs->Belt.List.map(compile_fun)->Belt.List.toArray)

    list{
      Call("main",0),
      Exit,
      ...funs_code
    }
  }

}