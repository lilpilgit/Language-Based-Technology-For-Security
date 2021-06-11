
type  ttype =
  | Tint
  | Tbool
  | Tfun of ttype * ttype;;


type expr =
  | CstI of int
  | CstB of bool
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * ttype * expr   
  | Call of expr * expr;;
  

type 'v env = (string * 'v) list;;

let rec lookup env x =
    match env with
    | []        -> failwith (x ^ " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let basicTypes = [
  "+", Tfun(Tint, Tfun(Tint, Tint));
  "-", Tfun(Tint, Tfun(Tint, Tint));
  "*", Tfun(Tint, Tfun(Tint, Tint));
  "<", Tfun(Tint, Tfun(Tint, Tbool));
  "and", Tfun(Tbool, Tfun(Tbool, Tbool));
  "or", Tfun(Tbool, Tfun(Tbool, Tbool))
];;


exception Type_error of string;;


let rec type_of gamma e =
  match e with
  | CstI(_) -> Tint
  | CstB(_) -> Tbool
  | Var(x)  -> lookup gamma x
  | Let(x, e1, e2) ->
    let t1 = type_of gamma e1 in
    type_of ((x,t1)::gamma) e2
  | If(e1, e2, e3) ->
    if (type_of gamma e1) = Tbool then
      let t2 = type_of gamma e2 in
      let t3 = type_of gamma e3 in
      if t2 = t3 then t2 else raise (Type_error "if branches have different types")
    else
      raise (Type_error "if with no a boolean guard")
  | Fun(x,tx, e) -> Tfun(tx, type_of ((x, tx) :: gamma) e)
  | Prim("=", e1, e2) ->
    let t1 = type_of gamma e1 in
    let t2 = type_of gamma e2 in
    begin
      match t1, t2 with
      | Tint, Tint
      | Tbool, Tbool -> Tbool
      | Tfun(_,_), Tfun(_,_) ->
        raise (Type_error "Error comparing functional values for equality")
      | _, _ -> raise (Type_error "error in the arguments of =")
    end
  | Prim(op, e1, e2) ->
    let t1 = type_of gamma e1 in
    let t2 = type_of gamma e2 in
    let top = lookup gamma op in
    begin
      match top with
      | Tfun(t1', Tfun(t2', tr')) ->
        if (t1' = t1 && t2' = t2) then
          tr'
        else
          raise (Type_error ("error in the arguments of " ^ op))
      | _ -> failwith "Inconsistent state"
    end
  | Call(e1, e2) ->
    let t1 = type_of gamma e1 in
    let t2 = type_of gamma e2 in
    begin
      match t1 with
      | Tfun(tx, tr) ->
        if tx = t2 then
          tr
        else
          raise (Type_error "fuctional application: argument type mismatch")
      | _ -> raise (Type_error "application to a non functional value")
end ;;

let type_check e = type_of basicTypes e;;


