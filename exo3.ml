(** Question 3.1 *)
let flatten l = List.fold_right (@) l [];;
let flatten2 l = List.fold_left (@) [] l;;

(** Question 3.2 *)
let sumcompose f x n =
    let rec sumcompose_rec f x n acc acc_f_pow = 
        if n = 0 then []
        else
            acc::sumcompose_rec f x (n-1) (acc+acc_f_pow) (acc_f_pow*f x)
    in

    sumcompose_rec f x n x (f x);;

let sumcompose2 f x n =
    let rec sumcompose2_rec f x n acc_l acc acc_f_pow = 
        if n = 0 then acc_l
        else
            sumcompose2_rec f x (n-1) (acc_l@[acc]) (acc+acc_f_pow) (acc_f_pow*f x)
    in

    sumcompose2_rec f x n [] x (f x);;

(** 
sumcompose -> int list. Car d'après l'énoncé la fonction doit renvoyer une liste d'entier, composer de la somme de f^k x avec k de 0 à n.
*)
