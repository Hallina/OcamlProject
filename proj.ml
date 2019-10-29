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

(* insertion d'un mot dans la liste : 
s'il y est déja, ajout de la clé à la liste de valeurs 
sinon création du mot et de la liste de valeurs *)
let rec insert_phi_in_list l p k =
	match l with
	| [] -> (p, k::[])::l
	| (phi, elems)::[] ->  if phi = p then (phi, k::elems)::[] else (phi, elems)::(p, k::[])::[]
	| h::t -> match h with
		| (phi, elems) -> if phi = p then (phi, k::elems)::t else h::insert_phi_in_list t p k;;

(* parcours de l'arbre pour constituer la liste de mots *)
let parcoursCompPhi abr = 
	let rec parcoursCompPhi_aux abr l =
		match abr with
		| Vide -> []
		| Noeud(Vide, k, Vide, p) -> insert_phi_in_list l p k
		| Noeud(left, k, right, p) -> insert_phi_in_list (parcoursCompPhi_aux right (parcoursCompPhi_aux left l)) p k
	in

	parcoursCompPhi_aux abr [];;


(* EXEMPLES/TESTS : *)
(* liste de base *)
let l = [4; 2; 3; 8; 1; 9; 6; 7; 5] in

(* abr associé *)
let t = list2abr l in

(* abr avec mots (styles ((())))() *)
let abrPhi = abr_phi t in

(* génération de la liste de mots avec valeurs associées  *)
affiche_phi (parcoursCompPhi abrPhi);;