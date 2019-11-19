(*2 - Compression des ABR*)

open Printf;;
open Abr;;
open Gen;;

type abr_phi = ABRP_Empty | ABRP_Node of (abr_phi * int * abr_phi * string);;

let print_abrPhi abr =
    let rec pap_aux abr =
        match abr with
        | ABRP_Empty -> ()
        | ABRP_Node(left, k, right, phi) -> 
            pap_aux left;
            printf "%d, %s; " k phi;
            pap_aux right
        in

    pap_aux abr;
    print_char '\n';;

(* question 2.4 *)
let rec phi abr =
    match abr with
    | ABR_Empty -> ""
    | ABR_Node(left, k, right) -> "(" ^ (phi left) ^ ")" ^ (phi right);;


(* créé un arbre dont les noeuds contiennent le mot associé aux sous-arbre *)
let rec abr2abrPhi abr =
    match abr with
    | ABR_Empty -> ABRP_Empty
    | ABR_Node(left, k, right) -> ABRP_Node((abr2abrPhi left), k, (abr2abrPhi right), (phi abr));;

(* affiche la liste de mots l *)
let rec print_phiList l =
    match l with
    | [] -> print_char '\n'
    | (phi, elems)::t -> print_string phi; print_int_list elems; printf "\n"; print_phiList t;;

(* renvoie les valeurs correspondant au mot p *)
let rec get_vals_of_phi l p =
    match l with
    | [] -> []
    | (phi, elems)::t -> if phi = p then elems else get_vals_of_phi t p;;

(* teste l'appartenance d'un mot à une liste *)
let rec not_contains phi l =
    match l with
    | [] -> true
    | (p, el)::t -> if p = phi then false else not_contains phi t;;

(* renvoie la liste des mots de l2 que l1 ne contient pas *)
let rec get_different_phis l1 l2 =
    match l2 with
    | [] -> []
    | (p, el)::t -> 
        if not_contains p l1 then (p, el)::(get_different_phis l1 t)
        else get_different_phis l1 t;;

(* fusionnes deux listes de mots *)
let merge_phi l1 l2 =
    let rec m_aux l1 l2 =
        match l1 with
        | [] -> []
        | (_, [])::_ -> []
        | (phi, elems)::t ->
            let elems2 = get_vals_of_phi l2 phi in
            (phi, elems@elems2)::(m_aux t l2)
    in

    let mergedList = (m_aux l1 l2) in
    mergedList@(get_different_phis mergedList l2);;

(* parcours de l'arbre pour constituer la liste de mots *)
let rec parcours_comp_phi abr = 
        match abr with
        | ABRP_Empty -> []
        | ABRP_Node(ABRP_Empty, k, ABRP_Empty, p) -> [(p, [k])]
        | ABRP_Node(ABRP_Empty, k, right, p) -> (p, [k])::(parcours_comp_phi right)
        | ABRP_Node(left, k, ABRP_Empty, p) -> (p, [k])::(parcours_comp_phi left)
        | ABRP_Node(left, k, right, p) -> (p, [k])::(merge_phi (parcours_comp_phi left) (parcours_comp_phi right));;

(* renvoie le mot associé à la valeur v *)
let rec get_phi_from_val v abrp =
    match abrp with
    | ABRP_Empty -> assert false (* v est forcément dans l'arbre *)
    | ABRP_Node(l, k, r, phi) -> 
        if v = k then phi
        else if v < k then get_phi_from_val v l
        else get_phi_from_val v r;;