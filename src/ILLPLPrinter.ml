open Util.Combinators

module S = ILLPLSyntax

let pr_list p_pre p_post p_sep p f l =
  let rec loop = function
    | [] -> ()
    | [x] -> Format.fprintf f "@[<2>%a@]" p x
    | x :: xs -> Format.fprintf f "@[<2>%a@]%a" p x p_sep ();
                 loop xs
  in Format.fprintf f "%a@[<hv>" p_pre ();
     loop l;
     Format.fprintf f "@]%a" p_post ()

let pr_str s f () = Format.fprintf f "%s" s

let pr_list_simple pre post sep p f l =
  let p_sep f () = Format.fprintf f "%s@ " sep in
  pr_list (pr_str pre) (pr_str post) p_sep p f l

let pr_list_literal p f l =
  pr_list_simple "[" "]" ";" p f l

let pr_paren_comma_list p f l =
  pr_list_simple "(" ")" "," p f l



let rec pr_expr f e =
  match e with
  | S.Unit -> Format.fprintf f "()"
  | S.Bool b -> Format.fprintf f "%s" (if b then "true" else "false")
  | S.Var s -> Format.fprintf f "%s" s
  | S.Num n -> Format.fprintf f "%d" n
  | S.List l -> pr_list_literal pr_expr f l
  | S.Let (x, e, body) -> Format.fprintf f "@[@[let@ %s@ =@ @ %a@@] in@ %a@]"
                                         x pr_expr e pr_expr body
  | S.Sequence (e1, e2) -> Format.fprintf f "@[%a;@ %a@]" pr_expr e1 pr_expr e2

  | S.If (e1, e2, e3) -> Format.fprintf f "@[<hv>@[if@ %a@ then@]@;<1 4>@[%a@]@ else@;<1 4>@[%a@]@]"
                                        pr_expr e1 pr_expr e2 pr_expr e3
  | S.And (e1, e2) -> Format.fprintf f "@[%a@ %s@ %a@]" pr_expr e1 "&&" pr_expr e2
  | S.Or (e1, e2) -> Format.fprintf f "@[%a@ %s@ %a@]" pr_expr e1 "||" pr_expr e2
  | S.Implies (e1, e2) -> Format.fprintf f "@[%a@ %s@ %a@]" pr_expr e1 "==>" pr_expr e2
  | S.Funcall (nm, args) -> Format.fprintf f "%s@[%a@]" nm pr_args args
  | S.Forall (x, e) -> Format.fprintf f "@[@[forall@ %s.@]@ @[%a@]@]" x pr_expr e
  | S.Exists (x, e) -> Format.fprintf f "@[@[exists@ %s.@]@ @[%a@]@]" x pr_expr e
  | S.Assert e -> Format.fprintf f "@[exists@ %a@]" pr_expr e
  | S.Length e -> Format.fprintf f "|@[%a@]|" pr_expr e
  | S.Index (e1, e2) -> Format.fprintf f "@[%a[%a]@]" pr_expr e1 pr_expr e2
  | S.Slice (e1, e2) -> Format.fprintf f "@[%a[%a..]@]" pr_expr e1 pr_expr e2
  | S.Unop (uop, e) -> Format.fprintf f "@[%s@ %a@]" (S.unop_to_string uop) pr_expr e
  | S.Binop (bop, e1, e2) -> Format.fprintf f "@[%a@ %s@ %a@]"
                                            pr_expr e1
                                            (S.binop_to_string bop)
                                            pr_expr e2
and pr_args f l = pr_paren_comma_list pr_expr f l

let pr_requires f (S.Requires e) =
  if e = S.Bool true then ()
  else Format.fprintf f "@ @[requires@ @[<2>%a@]@]" pr_expr e

let pr_ensures f (S.Ensures (x, e)) =
  if e = S.Bool true then ()
  else begin
      Format.fprintf f "@ @[ensures@ ";
      if x = "_" then () else Format.fprintf f "%s.@ " x;
      Format.fprintf f "@[<2>%a@]@]" pr_expr e
    end


let pr_var f s = Format.fprintf f "%s" s
let pr_vars f l = pr_paren_comma_list pr_var f l

let pr_binding f b =
  match b with
  | S.Fun (nm, args, req, ens, body) ->
     Format.fprintf f "@[@[<v4>@[fun@ %s@]%a%a%a@]@ =@;<1 4>%a@]"
                    nm
                    pr_vars args
                    pr_requires req
                    pr_ensures ens
                    pr_expr body


let pr_bindings f bs =
  let p_sep f () = Format.fprintf f "@,@," in
  pr_list (pr_str "") (pr_str "") p_sep pr_binding f bs

let pr_prog f (bs, e) =
  Format.fprintf f "@[<hv>@[<hv>%a@]@ @,@[val@ _@ =@ @[%a@]@]@]" pr_bindings bs pr_expr e

