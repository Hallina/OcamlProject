Random.self_init();;

let print_int_list l = 
    List.iter (fun x -> print_int x; print_char ';') l;
    print_char '\n';;

(*suppresion du nième élément d'une liste*)
let rec remove_at n l = 
    match l with
    | [] -> []
    | h::t -> if n = 0 then t else h::remove_at (n-1) t;;

(*question 1.1*)
let exctraction_alea l p = 
    let pos = Random.int (List.length l) in
    let elem = List.nth l pos in

    ((remove_at pos l), elem::p);;

(*initialisation d'une liste triée de 1 à n (Q1.2)*)
let rec init_list l n =
    if n = 1 then n::l else init_list (n::l) (n-1);;

(*vidage de L dans P (Q1.2)*)
let rec vidage l p acc =
    let res = exctraction_alea l p in
    if acc = 1 then snd res else vidage (fst res) (snd res) (acc-1);;

(*question 1.2*)
let gen_permutation n =
    vidage (init_list [] n) [] n;;

(* transforme une liste d'entier en string *)
let int_list2string l = 
    let rec il2s_aux l acc =
        match l with 
        | [] -> acc
        | h::t -> 
            il2s_aux t (acc ^ (string_of_int h) ^ ";")
    in

    il2s_aux l "";;