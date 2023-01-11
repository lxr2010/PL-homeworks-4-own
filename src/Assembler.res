module StackMachine = {
  type label = string

  type instr = Cst(int) | Add | Mul | Leq | Var(int) | Pop | Swap 
    | Label(label) | Call(label, int) | Ret(int) 
    | Goto(label) | IfZero(label)
    | Exit


  let toString = (instrs:array<instr>) => {
    let  toStringList = (instr) => switch instr {
    | Cst(i) => list{"Cst("++Js.Int.toString(i)++")"}
    | Add => list{"Add"}
    | Mul => list{"Mul"}
    | Leq => list{"Leq"}
    | Var(i) => list{"Var("++Js.Int.toString(i)++")"}
    | Pop => list{"Pop"}
    | Swap => list{"Swap"}
    | Label(l) => list{"Label("++l++")"}
    | Call(l,n) => list{"Call("++l++","++Js.Int.toString(n)++")"}
    | Ret(n) => list{"Ret("++Js.Int.toString(n)++")"}
    | Goto(l) => list{"Goto("++l++")"}
    | IfZero(l) => list{"IfZero("++l++")"}
    | Exit => list{"Exit"}
    }
    let outputList = Belt.List.concatMany(Belt.Array.map(instrs,toStringList))
    let listTailToString = (instr_strs: list<string>) : string => 
      List.fold_left((str,inst)=>str++";"++inst,"",instr_strs)
    switch outputList {  
      | list{} => "[]"
      | list{inst} => "[" ++ inst ++"]"
      | list{inst, ...rest} => "[" ++ inst ++ listTailToString(rest) ++ "]"
    }

  } 

  let size_of_instr = (instr: instr) : int => switch instr {
  | Cst(_) => 2 
  | Var(_) => 2
  | Label(_) => 0
  | Goto(_) => 2
  | IfZero(_) => 2
  | Call(_,_) => 3
  | Ret(_) => 2
  | _ => 1
  } 

  let encode = (instrs: array<instr>) : array<int> => {
    let position = ref(0)
    let label_map = Belt.HashMap.String.make(~hintSize=10)
    for cur in 0 to Belt.Array.length(instrs) -1 {
      // construct label map
      switch instrs[cur] {
      | Label(l) => Belt.HashMap.String.set(label_map,l,position.contents)
      | instr => position := position.contents + size_of_instr(instr)
      }
      // record the PC per earch label
    }
    let int_code:array<int> = Belt.Array.make(position.contents,0)
    position := 0
    for cur in 0 to Belt.Array.length(instrs) - 1 {
      // translate to int_code
      switch instrs[cur] {
      | Cst(i) => {
        int_code[position.contents] = 0
        int_code[position.contents+1] = i
      }
      | Add => {
        int_code[position.contents] = 1
      }
      | Mul => {
        int_code[position.contents] = 2
      }
      | Var(i) => {
        int_code[position.contents] = 3
        int_code[position.contents+1] = i
      }
      | Pop => {
        int_code[position.contents] = 4
      }
      | Swap => {
        int_code[position.contents] = 5
      }
      | Call(l, n) => {
        let label_addr = switch Belt.HashMap.String.get(label_map, l) {
        | Some (addr) => addr 
        | _ => assert false
        }
        int_code[position.contents] = 6
        int_code[position.contents+1] = label_addr
        int_code[position.contents+2] = n
      }
      | Ret(n) => {
        int_code[position.contents] = 7
        int_code[position.contents+1] = n
      }
      | IfZero(l) => {
        let label_addr = switch Belt.HashMap.String.get(label_map, l) {
        | Some (addr) => addr 
        | _ => assert false
        }
        int_code[position.contents] = 8
        int_code[position.contents+1] = label_addr 
      }
      | Goto(l) => {
        let label_addr = switch Belt.HashMap.String.get(label_map, l) {
        | Some (addr) => addr 
        | _ => assert false
        }
        int_code[position.contents] = 9
        int_code[position.contents+1] = label_addr
      }
      | Exit => {
        int_code[position.contents] = 10
      }
      | Leq => {
        int_code[position.contents] = 11
      }
      | Label(_) => {
        assert true
      }
      }
      position := position.contents + size_of_instr(instrs[cur])
    }

    int_code 
  }
  
};