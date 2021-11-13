let rec is_perm l1 l2 =
    match l1 with
    | [] -> l2 = []
    | head_l1::tail_l1 -> 
        match l2 with
        | [] -> false
        | head_l2::tail_l2 -> is_perm tail_l1 tail_l2

    
let l1 = [2; 3; 4; 3]
let l2 = [3; 3; 4; 2]
let l3 = [3; 4; 2]
let l4 = [2; 4; 3]
let l5 = []
let l6 = [1]

let () = assert (is_perm l6 l6)
let () = assert (is_perm l5 l5)
let () = assert (is_perm l1 l1)
let () = assert (is_perm l1 l2)
let () = assert (not (is_perm l2 l3))
let () = assert (is_perm l3 l4)
let () = assert (not (is_perm l4 l5))
