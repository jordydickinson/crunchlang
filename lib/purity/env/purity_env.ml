module Pure_expr = Purity.Pure_expr
module Expr = Purity.Expr
module Stmt = Purity.Stmt
module Decl = Purity.Decl

type binding = {
  typ: Type.t;
  pure: bool;
}

type t = {
  scopes: string Stack.t Stack.t;
  bindings: binding list String.Table.t
}

let enter_scope env =
  Stack.push env.scopes @@ Stack.create ()

let create () =
  let env = {
    scopes = Stack.create ();
    bindings = String.Table.create ();
  } in
  enter_scope env;
  env

let exit_scope env =
  let vars = Stack.pop_exn env.scopes in
  Stack.until_empty vars begin fun ident ->
    Hashtbl.remove_multi env.bindings ident
  end

let scoped env ~f =
  enter_scope env;
  protect ~f
    ~finally:(fun () -> exit_scope env)

let bind' env ~ident ~binding =
  Stack.push (Stack.top_exn env.scopes) ident;
  Hashtbl.add_multi env.bindings ~key:ident ~data:binding

let bind env ~ident ~typ ~pure =
  bind' env ~ident ~binding:{ typ; pure }

let lookup env ident =
  match List.hd @@ Hashtbl.find_multi env.bindings ident with
  | None -> error "Unbound identifier" ident String.sexp_of_t
  | Some binding -> Ok binding
