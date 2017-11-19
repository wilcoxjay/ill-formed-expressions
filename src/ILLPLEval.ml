open Util.Combinators

module S = ILLPLSyntax

type value =
  | Unit
  | Bool of bool
  | Num of int
  | List of value list

let value_eq _ (v1 : value) _ (v2 : value) = Bool (v1 = v2)

let value_add m1 (v1 : value) m2 (v2 : value) =
  match v1, v2 with
  | Num n1, Num n2 -> Num (n1 + n2)
  | List vs1, List vs2 -> List (vs1 @ vs2)
  | _ -> failwith ("value_add: " ^ m1 () ^ " " ^ m2 ())

let rec value_to_expr v =
  match v with
  | Unit -> S.Unit
  | Bool b -> S.Bool b
  | Num n -> S.Num n
  | List l -> S.List (List.map value_to_expr l)

let pr_value f v =
  ILLPLPrinter.pr_expr f (value_to_expr v)

module Env : sig
  type t

  exception UnboundVariable of S.var
  exception UnboundFunction of S.funname

  val of_bindings : S.binding list -> t
  val clear_vars : t -> t
  val bind_var : t -> S.var -> value -> t
  val bind_vars : S.var list -> value list -> t -> t
  val lookup_var : t -> S.var -> value
  val lookup_fun : t -> S.funname -> S.binding
end =
struct

type ('a, 'b) map = ('a * 'b) list

type t = Env of (S.var, value) map * (S.funname, S.binding) map

let of_bindings bs =
  Env ([], List.map (fun (S.Fun (nm, _, _, _, _) as b) -> (nm, b)) bs)

let clear_vars (Env (_, fs)) = Env ([], fs)
let bind_var (Env (vs, fs)) x v = Env ((x, v) :: vs, fs)
let bind_vars xs vs env =
  List.fold_left2 bind_var env xs vs

exception UnboundVariable of S.var
exception UnboundFunction of S.funname

let lookup_var (Env (vs, _)) v =
  try List.assoc v vs
  with _ -> raise (UnboundVariable v)

let lookup_fun (Env (_, fs)) f =
  try List.assoc f fs
  with _ -> raise (UnboundFunction f)

end

let ets e () =
  Format.asprintf "%a" ILLPLPrinter.pr_expr e


let of_bool msg = function
  | (Bool b) -> b
  | _ -> failwith ("of_bool: " ^ msg ())

let of_int msg = function
  | (Num n) -> n
  | _ -> failwith ("of_bool: " ^ msg ())

let of_list msg = function
  | (List l) -> l
  | _ -> failwith ("of_list: " ^ msg ())

let safe_index msg l i =
  try List.nth l i
  with _ -> failwith (Printf.sprintf "safe_index: %s[%d]" (msg ()) i)

let rec safe_slice msg l i =
  if i < 0 then failwith (Printf.sprintf "safe_slice too small %s[%d..]" (msg ()) i)
  else if i = 0 then l
  else match l with
       | [] -> failwith (Printf.sprintf "safe_slice too big %s[%d..]" (msg ()) i)
       | x :: l -> safe_slice msg l (i - 1)


let wrapped_not msg x = Bool (not (of_bool msg x))

let denote_unop = function
  | S.Not -> wrapped_not
  | S.Neg -> (fun msg x -> Num (- (of_int msg x)))

let wrap_int_int_int f m1 e1 m2 e2 =
  Num (f (of_int m1 e1) (of_int m2 e2))

let wrap_int_int_bool f m1 e1 m2 e2 =
  Bool (f (of_int m1 e1) (of_int m2 e2))

let denote_binop = function
  | S.Add -> value_add
  | S.Sub -> wrap_int_int_int (  -  )
  | S.Mul -> wrap_int_int_int (  *  )
  | S.Div -> wrap_int_int_int (  /  )
  | S.Eq  -> value_eq
  | S.Neq -> fun m1 e1 m2 e2 -> wrapped_not (fun () -> failwith "impossible")
                                            (value_eq m1 e1 m2 e2)
  | S.Lt  -> wrap_int_int_bool (  <  )
  | S.Le  -> wrap_int_int_bool (  <= )
  | S.Gt  -> wrap_int_int_bool (  >  )
  | S.Ge  -> wrap_int_int_bool (  >= )

let rec eval_expr env e =
  try
  match e with
  | S.Unit                  -> Unit
  | S.Bool b                -> Bool b
  | S.Var x                 -> Env.lookup_var env x
  | S.Num n                 -> Num n
  | S.List l                -> List (List.map (eval_expr env) l)
  | S.Let (x, e, body)      -> eval_expr (Env.bind_var env x (eval_expr env e)) body
  | S.Sequence (e1, e2)     -> ignore (eval_expr env e1); eval_expr env e2
  | S.If (e1, e2, e3)       -> if of_bool (ets e1) (eval_expr env e1)
                               then eval_expr env e2
                               else eval_expr env e3
  | S.And (e1, e2)          -> Bool (of_bool (ets e1) (eval_expr env e1) &&
                                     of_bool (ets e2) (eval_expr env e2))
  | S.Or (e1, e2)           -> Bool (of_bool (ets e1) (eval_expr env e1) ||
                                     of_bool (ets e2) (eval_expr env e2))
  | S.Implies (e1, e2)      -> if of_bool (ets e1) (eval_expr env e1)
                               then eval_expr env e2
                               else Bool true
  | S.Funcall (f, args)     -> eval_funcall env f args
  | S.Forall (x, e)         -> failwith "eval_expr: tried to evaluate forall."
  | S.Exists (x, e)         -> failwith "eval_expr: tried to evaluate exists."
  | S.Assert e              -> if of_bool (ets e) (eval_expr env e) then Unit
                               else failwith (Format.asprintf "eval_expr: assertion failed. %a"
                                                              ILLPLPrinter.pr_expr e)
  | S.Length e              -> Num (List.length (of_list (ets e) (eval_expr env e)))
  | S.Index (e1, e2)        -> safe_index (ets e1)
                                          (of_list (ets e1) (eval_expr env e1))
                                          (of_int (ets e2) (eval_expr env e2))
  | S.Slice (e1, e2)        -> List (safe_slice (ets e1)
                                          (of_list (ets e1) (eval_expr env e1))
                                          (of_int (ets e2) (eval_expr env e2)))
  | S.Unop (unop, e)        -> denote_unop unop (ets e) (eval_expr env e)
  | S.Binop (binop, e1, e2) -> denote_binop binop (ets e1) (eval_expr env e1)
                                                  (ets e2) (eval_expr env e2)
  with Failure msg ->
    failwith (Format.asprintf "%s\n%a" msg ILLPLPrinter.pr_expr e)

and eval_funcall env f args =
  let (S.Fun (_, xs, _, _, body)) = Env.lookup_fun env f in
  let env' =
    env |> Env.clear_vars
        |> Env.bind_vars xs (List.map (eval_expr env) args) in
  eval_expr env' body


let eval_prog (bs, e) =
  eval_expr (Env.of_bindings bs) e

