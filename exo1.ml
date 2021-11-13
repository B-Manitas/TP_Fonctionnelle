let x y z = 
let x =
let z = 
y - z in (** Lieurs de y et z: ligne 1 *)
let x y z = 
z - y in (** Lieurs de y et z: ligne 5 *)
x y z in (** Lieur de x: ligne 5. Lieur de y: ligne 1. Lieur de z: Ligne 3 *)
x + y (** Lieurs de x: ligne 2. Lieur de y: ligne 1 *)

(** La fonction x, ci dessus, calcule la valeur de la difference entre les parametres y et z de la fonction x principal. *)


(** Question 1.2 *)
(** Calcule la somme des 2 arguments. *)
let plus_moins x y =
    let g = fun u -> - u in
    match x with
    | 0 -> g (0 + y)
    | x -> g (0 + (g y) - x)

(** Calcule la valeur du premier argument moins le second *)
let plus_moins_2 x y =
    let g = fun u -> - u in
    match x with
    | 0 -> g (0 + y)
    | x -> (g 0 + (g (y - x)))


(** Question 1.3 *)
(** 
l=              |g=                 |s=                 |h l= 
[4]             |pas de valeurs     |pas de valeurs     |[9]
[3, 4]          |[9]                |3                  |[3; 9]
[7, 3, 4]       |[3; 9]             |4                  |[4; 3; 9]
[1, 7, 3, 4]    |[4; 3; 9]          |1                  |[1; 4; 3; 9]
[5, 1, 7, 3, 4] |[1; 4; 3; 9]       |4                  |[4; 1; 4; 3; 9]
*)

(**
b)
La fonction f renvoie une liste dont la premiere valeur correspond a celle de s. Puis, le reste de la liste correspond a liste g.
 *)
