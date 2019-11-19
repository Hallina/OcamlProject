(* 1.1 Synthèse de données *)

type abr = ABR_Empty | ABR_Node of (abr * int * abr);;

(* affichage d'un abr (parcours infixe) *)
let print_abr abr =
    let rec pa_aux abr =
        match abr with
        | ABR_Empty -> ()
        | ABR_Node(left, k, right) ->   
            pa_aux left;
            print_int k; print_string ", ";
            pa_aux right
        in

    pa_aux abr;
    print_char '\n';;
 
(* ajout d'une feuille dans un arbre (Q1.3) *)
let rec add n abr = 
    match abr with
    | ABR_Empty -> ABR_Node(ABR_Empty, n, ABR_Empty)
    | ABR_Node(left, k, right) -> 
        if n = k then abr
        else if n < k then ABR_Node((add n left), k, right)
        else ABR_Node(left, k, (add n right));;

(* question 1.3 *)
let list2abr l =
    let rec l2a_aux l abr = 
        match l with
        | [] -> abr
        | h::t -> l2a_aux t (add h abr)
    in

    l2a_aux l ABR_Empty;;

(* recherche dans un ABR *)
let rec abr_find abr n =
    match abr with
    | ABR_Empty -> false
    | ABR_Node(fg, v, fd) ->
        if n = v then true
        else if n < v then abr_find fg n
        else abr_find fd n