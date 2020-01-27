open Llvm

type binding =
  | Let of {
      value: llvalue;
      typ: Type.t;
    }
  | Var of {
      pointer: llvalue;
      typ: Type.t
    }

type t = {
  scopes: string Stack.t Stack.t;
  bindings: binding list String.Table.t
}

let enter_scope env =
  Stack.push env.scopes @@ Stack.create ()

let exit_scope env =
  let vars = Stack.pop_exn env.scopes in
  Stack.until_empty vars begin fun ident ->
    Hashtbl.remove_multi env.bindings ident
  end

let bind env ~ident ~binding =
  Stack.push env.scopes @@ Stack.create ();
  Hashtbl.add_multi env.bindings ~key:ident ~data:binding

let bind_let env ~ident ~typ ~value =
  let binding = Let { value; typ } in
  bind env ~ident ~binding

let bind_var env ~ident ~typ ~pointer =
  let binding = Var { pointer; typ } in
  bind env ~ident ~binding

let lookup env ident =
  Hashtbl.find_multi env.bindings ident
  |> List.hd_exn

let create () =
  let env = {
    scopes = Stack.create ();
    bindings = String.Table.create ()
  } in
  enter_scope env;
  env
