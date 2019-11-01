(*1.1 Synthèse de données*)

(* Random.self_init() *)
Random.init 100;;

(*affichage d'une liste d'entier*)
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
	let l = init_list [] n in
	let p = [] in

	print_string "gen_permutation - liste initiale : "; print_int_list l;

	let res = vidage l p n in

	print_string "gen_permutation - liste finale : "; print_int_list res;

	res;;

(*1.2 Construction de l'ABR*)

type 'a abr = Empty | Node of ('a abr * 'a * 'a abr);;

(*affichage d'un abr (parcours infixe)*)
let print_abr abr =
	let rec print_abr_aux abr =
		match abr with
		| Empty -> ()
		| Node(left, k, right) ->	print_abr_aux left;
									print_int k; print_string ", ";
									print_abr_aux right
		in

	print_abr_aux abr;
	print_char '\n';;

(*ajout d'une feuille dans un arbre (Q1.3)*)
let rec add n abr = 
	match abr with
	| Empty -> Node(Empty, n, Empty)
	| Node(left, k, right) -> 
		if n = k then abr
		else if n < k then Node((add n left), k, right)
		else Node(left, k, (add n right));;

(*question 1.3*)
let list2abr l =
	let rec list2abr_aux l abr = 
		match l with
		| [] -> abr
		| h::t -> list2abr_aux t (add h abr)
	in

	list2abr_aux l Empty;;

let l = gen_permutation 10 in
let t = list2abr l in
print_abr (t);; 

(*2 - Compression des ABR*)

(* question 2.4 *)
let rec phi abr =
	match abr with
	| Empty -> ""
	| Node(left, k, right) -> "(" ^ (phi left) ^ ")" ^ (phi right);;

type 'a abrPhi = Vide | Noeud of ('a abrPhi * 'a * 'a abrPhi * string);;

let rec abr_phi abr =
	match abr with
	| Empty -> Vide
	| Node(left, k, right) -> Noeud((abr_phi left), k, (abr_phi right), (phi abr));;

(* affichage des mots situés dans les noeuds *)
let rec affiche_phi l =
	match l with
	| [] -> print_char '\n'
	| (phi, elems)::t -> print_string phi; print_int_list elems; affiche_phi t;;

(* renvoie la liste de valeurs associées au mot phi *)
let rec get_vals_of_phi l p =
    match l with
    | [] -> []
    | (phi, elems)::t -> if phi = p then elems else get_vals_of_phi t p;;

(* vérifie si l contient le mot phi *)
let rec not_contains phi l =
    match l with
    | [] -> true
    | (p, el)::t -> if p = phi then false else not_contains phi t;;

(* renvoie la liste des mots de l2 qui ne sont pas dans l1 *)
let rec get_different_phis l1 l2 =
    match l2 with
    | [] -> []
    | (p, el)::t -> 
        if not_contains p l1 then (p, el)::(get_different_phis l1 t)
        else get_different_phis l1 t;;

(* fusionne deux listes de mots ensemble 
	("()", [1;5])::("()()", [3]) et 
	("()", [7])::("(())", [8]) donnent
	("()", [1;5;7])::("()()", [3]) *)
let fusionne_phi l1 l2 =
    let rec fp_aux l1 l2 =
        match l1 with
        | [] -> []
        | (_, [])::_ -> []
        | (phi, elems)::t ->
            let elems2 = get_vals_of_phi l2 phi in
            (phi, elems@elems2)::(fp_aux t l2)
    in

    let mergedList = (fp_aux l1 l2) in
    mergedList@(get_different_phis mergedList l2);;

(* parcours de l'arbre pour constituer la liste de mots *)
let rec parcours_comp_phi abr = 
    match abr with
    | Vide -> []
    | Noeud(Vide, k, Vide, p) -> (p, k::[])::[]
    | Noeud(Vide, k, right, p) -> (p, k::[])::(parcours_comp_phi right)
    | Noeud(left, k, Vide, p) -> (p, k::[])::(parcours_comp_phi left)
    | Noeud(left, k, right, p) -> (p, k::[])::(fusionne_phi (parcours_comp_phi left) (parcours_comp_phi right));;


(* EXEMPLES/TESTS : *)
(* liste de base *)
let l = [4; 2; 3; 8; 1; 9; 6; 7; 5] in

(* abr associé *)
let t = list2abr l in

(* abr avec mots (styles ((())))() *)
let abrPhi = abr_phi t in

(* génération de la liste de mots avec valeurs associées  *)
affiche_phi (parcoursCompPhi abrPhi);;