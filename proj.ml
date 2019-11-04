(* 1.1 Synthèse de données *)

Random.self_init();
open Printf;;
(*Random.init 100;*)

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
    vidage (init_list [] n) [] n;;

(*1.2 Construction de l'ABR*)

type abr = E | N of (abr * int * abr);;

(*affichage d'un abr (parcours infixe)*)
let print_abr abr =
    let rec print_abr_aux abr =
        match abr with
        | E -> ()
        | N(left, k, right) ->   
            print_abr_aux left;
            print_int k; print_string ", ";
            print_abr_aux right
        in

    print_abr_aux abr;
    print_char '\n';;

(*ajout d'une feuille dans un arbre (Q1.3)*)
let rec add n abr = 
    match abr with
    | E -> N(E, n, E)
    | N(left, k, right) -> 
        if n = k then abr
        else if n < k then N((add n left), k, right)
        else N(left, k, (add n right));;

(*question 1.3*)
let list2abr l =
    let rec list2abr_aux l abr = 
        match l with
        | [] -> abr
        | h::t -> list2abr_aux t (add h abr)
    in

    list2abr_aux l E;;


(*2 - Compression des ABR*)

type 'a abrPhi = Vide | Noeud of ('a abrPhi * 'a * 'a abrPhi * string);;

let print_abrPhi abr =
    let rec pap_aux abr =
        match abr with
        | Vide -> ()
        | Noeud(left, k, right, phi) -> 
            pap_aux left;
            printf "%d, %s; " k phi;
            pap_aux right
        in

    pap_aux abr;
    print_char '\n';;

(* question 2.4 *)
let rec phi abr =
    match abr with
    | E -> ""
    | N(left, k, right) -> "(" ^ (phi left) ^ ")" ^ (phi right);;

(* créé un arbre dont les noeuds contiennent le mot associé aux sous-arbre *)
let rec abr2abrPhi abr =
    match abr with
    | E -> Vide
    | N(left, k, right) -> Noeud((abr2abrPhi left), k, (abr2abrPhi right), (phi abr));;

(* affiche la liste de mots l *)
let rec print_phiList l =
    match l with
    | [] -> print_char '\n'
    | (phi, elems)::t -> print_string phi; print_int_list elems; print_phiList t;;

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
        | Vide -> []
        | Noeud(Vide, k, Vide, p) -> [(p, [k])]
        | Noeud(Vide, k, right, p) -> (p, [k])::(parcours_comp_phi right)
        | Noeud(left, k, Vide, p) -> (p, [k])::(parcours_comp_phi left)
        | Noeud(left, k, right, p) -> (p, [k])::(merge_phi (parcours_comp_phi left) (parcours_comp_phi right));;

(* renvoie les valeurs associées au mot *)
let get_elems phi phiList =
    let rec ge_aux phi phiList =
        match phiList with
        | [] -> []
        | (p, elems)::t ->  if p = phi then elems else ge_aux phi t
    in

    ge_aux phi phiList;;

type  abr_compressed_list = 
    | ACL_Empty
    | ACL_Transition of int * abr_compressed_list ref
    | ACL_Node of branch list and branch = {mutable fg: abr_compressed_list; mutable lbl: int list; mutable fd: abr_compressed_list};;

(* générateur d'identifiant *)
class identifier =
    object
      val mutable id = 0
      method get_id = id
      method new_id = id <- id + 1; id
    end;;

(* convertit une liste d'entiers en noeud (liste de branches) *)
let elems2node_list phi elems phiHash =
    let rec e2ell_aux elems el =
        match elems with
        | [] -> el
        | h::t -> e2ell_aux t ({fg = ACL_Empty; lbl = [h]; fd = ACL_Empty}::el)
    in

    let res = List.rev (e2ell_aux elems []) in
    Hashtbl.add phiHash phi (ref res);

    res;;

(*  ajout d'un noeuc à un ACL, 
    un pointeur vers le noeud est stocké dans une hash map dont la clé est le mote correspondant au noeud *)
let rec acl_add phi elems acl phiHash = 
    match acl with
    | ACL_Transition (_) | ACL_Node([]) -> assert false
    | ACL_Empty -> ACL_Node((elems2node_list phi elems phiHash))
    | ACL_Node({fg; lbl; fd}::t) ->
        let k = List.hd lbl in
        let n = List.hd elems in

        if n = k then acl
        else if n < k then ACL_Node({fg = (acl_add phi elems fg phiHash); lbl = lbl; fd = fd}::t)
        else ACL_Node({fg = fg; lbl = lbl; fd = (acl_add phi elems fd phiHash)}::t);;

(* transforme la liste des mots en arbre *)
let phiList2acl l phiHash =
    let rec list2acl_aux l acl = 
        match l with
        | [] -> acl
        | (phi, elems)::t -> 
            printf "inserting %d\n" (List.hd elems);

            let newAcl = acl_add phi elems acl phiHash in
            list2acl_aux t newAcl;
    in
    list2acl_aux l ACL_Empty;;

(* renvoie la valeur du noeud précédent (dans l'abr) *)
let rec get_valPred n abr =
    match abr with
    | Vide -> assert false
    | Noeud(l, k, r, p) ->
        if n < k then
            match l with
            | Vide -> assert false
            | Noeud(ll, lk, lr, rp) -> 
                if n = lk then k
                else get_valPred n l

        else 
            match r with
                | Vide -> assert false
                | Noeud(rl, rk, rr, rp) -> 
                    if n = rk then k
                    else get_valPred n r;;

(* renvoie le mot associé à la valeur v *)
let rec get_phi_from_val v abrp =
    match abrp with
    | Vide -> assert false (* v est forcément dans l'arbre *)
    | Noeud(l, k, r, phi) -> 
        if v = k then phi
        else if v < k then get_phi_from_val v l
        else get_phi_from_val v r;;

(* ranvoie la branche qui contient la valeur v dans le noeud n *)
let rec get_branch_from_node v n =
    match n with
    | [] -> assert false
    | ({fg; lbl; fd} as branch)::t -> 
        if List.hd lbl = v then branch else get_branch_from_node v t;;

(* ajoute les étiquettes de transition à une branche *)
let add_transitions_to_branch branch toGetThere newTransition =
    let valBranch = List.hd branch.lbl in

    branch.lbl <- valBranch::newTransition::toGetThere;;

(* créé les transition branche vers noeud *)
let rec build_links node branchNode abrp map id =
    match node with
    | [] -> ();
    | ({fg; lbl; fd} as branch)::t ->
        let valBranch = List.hd lbl in
        let valPred = get_valPred (List.hd lbl) abrp in
        let phiPred = get_phi_from_val valPred abrp in
        let nodePred = !(Hashtbl.find map phiPred) in 

        printf "Linking node %d to node containing %d.\n" valPred valBranch;

        let branchPred = get_branch_from_node valPred nodePred in
        let newTransition = id#get_id in

        if valBranch < valPred then branchPred.fg <- ACL_Transition(newTransition, ref branchNode)
        else branchPred.fd <- ACL_Transition(newTransition, ref branchNode);

        add_transitions_to_branch branch (List.tl branchPred.lbl) newTransition;
        build_links t branchNode abrp map id;
    ;;

(*  construit l'arbre compressé 
    (construction des noeuds présents dans phiList puis chaînage) *)
let get_acl abrPhi phiList =
    let phiHash = Hashtbl.create 15 in
    let id = new identifier in
    let acl = phiList2acl phiList phiHash in

    let rec ab_aux acl =
        match acl with
        | ACL_Node([]) -> assert false
        | ACL_Transition(_)
        | ACL_Empty -> ()
        | ACL_Node({fg; lbl; fd}::nextBranch) ->
            build_links nextBranch acl abrPhi phiHash id;
            ab_aux fg;
            ab_aux fd;
    in

    ab_aux acl;
    acl;;

(* compare deux listes d'entiers entre elles *)
let rec compare_transitions t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | [], _ | _, [] -> false
    | h1::t1, h2::t2 -> if h1 = h2 then compare_transitions t1 t2 else false;;

 (* renvoie la branche correspondant aux transitions *)
 let rec get_branch_by_transitions transitions acl =
    match acl with
    | ACL_Empty | ACL_Transition(_) | ACL_Node([]) -> assert false
    | ACL_Node(({fg; lbl; fd})::nextBranch) ->
        if compare_transitions (List.tl lbl) transitions then acl
        else get_branch_by_transitions transitions (ACL_Node(nextBranch))

(* recherche dans un ACL *)
let rec acl_search v acl =
    let rec as_aux acl transitions =
        match acl with
        | ACL_Node([]) -> assert false
        | ACL_Empty -> ACL_Empty

        | ACL_Transition(t, r) -> 
            let branch = get_branch_by_transitions (t::transitions) !r in
            as_aux branch []; 

        | ACL_Node(({fg; lbl; fd})::_) as node ->
            let valBranch = List.hd lbl in

            if v = valBranch then node
            else if v < valBranch then as_aux fg (List.tl lbl)
            else as_aux fd (List.tl lbl);
    in

    as_aux acl [];;

let l = gen_permutation 30 in 

printf "Liste initiale :\n";
print_int_list l;

let abr = list2abr l in
let abrPhi = abr2abrPhi abr in

let phiList = parcours_comp_phi abrPhi in 

printf "phiList :\n";
print_phiList phiList;

let acl = get_acl abrPhi phiList in
let valToSearch = 12 in
let node = acl_search 12 acl in

if node != ACL_Empty then printf "La valeur %d se trouve dans l'ACL.\n" valToSearch
else printf "La valeur %d ne se trouve pas dans l'ACL.\n" valToSearch;

exit 0;;