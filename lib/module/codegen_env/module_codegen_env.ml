open Llvm

type t = {
  scopes: string Stack.t Stack.t;
  bindings: (llvalue * Type.t) list String.Table.t
}

let enter_scope env =
  Stack.push env.scopes @@ Stack.create ()

let exit_scope env =
  let vars = Stack.pop_exn env.scopes in
  Stack.until_empty vars begin fun ident ->
    Hashtbl.remove_multi env.bindings ident
  end

let bind env ~ident ~typ ~value =
  Stack.push env.scopes @@ Stack.create ();
  Hashtbl.add_multi env.bindings ~key:ident ~data:(value, typ)

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
