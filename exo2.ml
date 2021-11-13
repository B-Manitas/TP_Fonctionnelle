(** Question 2.1 *) 
let f1 x (y, z) = 
    match x with
    | 0 -> y = z
    | _ -> fst y;;

(**
x -> int. Car on observe qu'a la ligne 3, x est compare a une valeur. Or on peut voir qu'a la ligne 4, x est compare a 0. Par consequent, x est un entier.
(y, z) -> (bool * a). Car on peut voir qu'a la ligne 4 on compare les valeurs z et y. 
Donc au moins l'un des 2 est un boolean. De plus, la ligne 5 renvoie un element de type a.

f1 -> bool. Car on peut voir qu'a la ligne 4, la fonction renvoie le boolean y = z. Donc f1 renvoie un bool.
*)

let f2 x y =
    if x then [x]::y 
    else  [];;
(**
x -> bool. Car d'apres la ligne 16 on test la valeur du boolean x.
y -> bool list list. Car d'apres la ligne 16 on ajoute a la liste y des elements de type bool.
f2 -> bool list list. Car on renvoie le même type que y, d'après la ligne 16.
 *)

(** Question 2.2 *) 
(** La curryfication est le fait de transformer une fonction a plusieurs parametre en une fonction a un seul parametre retournant une fonction.*)
 
let rec not_list l =
    match l with
        |[] -> l
        |head::tail -> (not(head))::(not_list tail);;

(** Question 2.3 *)
type 'a arbre_binaire =
    | Feuille
    | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire ;;

let rec iter_arbre ab f =
    match ab with
    | Feuille -> ()
    | Noeud(e, g, d) -> f e; iter_arbre g f; iter_arbre d f;;

let rec compte_arbre ab f =
    match ab with
    | Feuille -> 0
    | Noeud(e, g, d) -> 
        if f e then 1 + compte_arbre g  f + compte_arbre d f
        else compte_arbre g f + compte_arbre d f

(**
iter_arbre -> unit. Car le parametre f est une fonction de type unit. Donc elle ne modifie pas l'arbre.  
compte_arbre -> int. Car calcule le nombre d'elements dans l'arbre verifiant le fonction predicat f passe en parametre.
*)