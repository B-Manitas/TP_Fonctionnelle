(** Question 4.1 *)
type op = Plus | Moins | Mult | Div
type ast = 
    | Valeur of int
    | MoinsUnaire of ast
    | Operation of ast * op * ast

(** Question 4.2 *)
(* 1 + 2) + 3 *)
let e1 = Operation(
    Operation(Valeur(1), Plus, Valeur(2)),
    Plus, 
    Valeur(3))

(* (12 * 7) - ((2/4) + 7) *)
let e2 = Operation(
    Operation(Valeur(12), Mult, Valeur(7)),
    Moins,
    Operation(
        Operation(Valeur(2), Div, Valeur(4)), 
        Plus, 
        Valeur(7))
)

(* 
(((3 × 4) + ((18 / 3) × 4)) + (10 + 4)) − 8 
*)

let e3 = 
    Operation(
        Operation(
            Operation(
                Operation(Valeur(3), Mult, Valeur(4)), 
                Plus, 
                Operation(
                    Operation(Valeur(18), Div, Valeur(3)), 
                    Mult, 
                    Valeur(4))
            ),
            Plus,
            Operation(Valeur(10), Plus, Valeur(4))
        ),
        Moins,
        Valeur(8)
    )

(** Question 4.3 *)
let print_op op =
    match op with
    | Plus -> Printf.printf " + "
    | Moins -> Printf.printf " - "
    | Mult -> Printf.printf " * "
    | Div -> Printf.printf " / "

let rec to_string ast =
    match ast with
    | Valeur x -> Printf.printf "%d" x
    | MoinsUnaire v -> Printf.printf "-"; to_string v
    | Operation (v1, op, v2) -> 
        Printf.printf "("; 
        to_string v1;
        print_op op;
        to_string v2;
        Printf.printf ")"

(** Question 4.4 *)
let eval_op v1 op v2 =
    match op with
    | Plus -> v1 + v2
    | Moins -> v1 - v2
    | Mult -> v1 * v2
    | Div -> v1 / v2

let rec eval ast =
    match ast with
    | Valeur v -> v
    | MoinsUnaire e -> -(eval e)
    | Operation(v1, op, v2) -> eval_op (eval v1) op (eval v1)

(** Question 4.5 *)
let check3_op op = 
    match op with
    | Plus -> true
    | Moins -> false
    | Mult -> true
    | Div -> false

let rec check3 ast =
    match ast with
    | Valeur v -> v >= 0 && v <= 2
    | MoinsUnaire e -> false
    | Operation(v1, op, v2) -> check3 v1 && check3_op op && check3 v2;;

(** Question 4.6 *)
let eval3_op v1 op v2 =
    match op with
    | Plus -> (v1 + v2) mod 3
    | Mult -> (v1 * v2) mod 3
    | _ -> raise (Invalid_argument "Operation Interdite Modulo 3.")


let rec eval3 ast =
    if check3 ast then
        match ast with
        | Valeur v -> v mod 3
        | MoinsUnaire e -> -(eval3 e)
        | Operation(v1, op, v2) -> eval3_op (eval3 v1) op (eval3 v1)
    
    else
        raise (Invalid_argument "L'arbre ne vérifie pas les conditions modulo 3.")