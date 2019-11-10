open Printf;;

Random.self_init();;

let tailleListe = 1000;;

let print_int_list l = 
    List.iter (fun x -> print_int x; print_char ';') l;;

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

let int_list2string l = 
    let rec il2s_aux l acc =
        match l with 
        | [] -> acc
        | h::t -> 
            il2s_aux t (acc ^ (string_of_int h) ^ ";")
    in

    il2s_aux l "";;

(* 1.1 Synthèse de données *)

type abr = E | N of (abr * int * abr);;

(*affichage d'un abr (parcours infixe)*)
let print_abr abr =
    let rec pa_aux abr =
        match abr with
        | E -> ()
        | N(left, k, right) ->   
            pa_aux left;
            print_int k; print_string ", ";
            pa_aux right
        in

    pa_aux abr;
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
    let rec l2a_aux l abr = 
        match l with
        | [] -> abr
        | h::t -> l2a_aux t (add h abr)
    in

    l2a_aux l E;;


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
    | ACL_Transition of int * node ref
    | ACL_Node of node 
        and node = {mutable fg: abr_compressed_list; branches: branch list; mutable fd: abr_compressed_list}
        and branch = {mutable values: int list};;

(* générateur d'identifiant *)
class identifier =
    object
      val mutable id = 0
      method get_id = id
      method new_id = id <- id + 1; id
    end;;

let print_branches node =
    let rec pb_aux branchList =
        match branchList with
        | [] -> ()
        | branch::nextBranches -> printf "%d;" (List.hd branch.values); pb_aux nextBranches
    in

    pb_aux node.branches;;

let rec print_acl acl =
    match acl with
    | ACL_Empty -> printf "vide \n";
    | ACL_Transition(t, r) -> printf "Transition %d to " t; print_acl (ACL_Node(!r));
    | ACL_Node(node) -> 
        printf "Node ";
        print_branches node;
        printf "\nfg : ";
        print_acl node.fg;

        printf "\nfd : ";
        print_acl node.fd;; 

(* convertit une liste d'entiers en liste de branches *)
let elems2node_list elems =
    let rec e2ell_aux elems el =
        match elems with
        | [] -> el
        | h::t -> e2ell_aux t ({values = [h]}::el)
    in

    List.rev (e2ell_aux elems []);;

(*  ajout d'un noeud à un ACL, 
    un pointeur vers le noeud est stocké dans une hash map dont la clé est le mot correspondant au noeud *)
let rec add_acl acl phi elems map =

    (* met à jour le parent du nouveau noeud (fonction auxiliare pour factoriser le code) *)
    let aa_aux son parent isLeft =
        match son with
        | ACL_Transition(_) -> assert false
        | ACL_Node(_) -> add_acl son phi elems map
        | ACL_Empty -> 
            let newNode = {fg = ACL_Empty; branches = (elems2node_list elems); fd = ACL_Empty} in
            
            Hashtbl.add map phi (ref newNode);

            if isLeft then parent.fg <- ACL_Node(newNode)
            else parent.fd <- ACL_Node(newNode)
    in

    match acl with
    | ACL_Empty | ACL_Transition(_) -> assert false
    | ACL_Node(node) ->
        if List.hd elems < List.hd (List.hd node.branches).values then aa_aux node.fg node true
        else aa_aux node.fd node false;;

(* transforme la liste des mots en arbre *)
let phiList2acl l map =
    
    (* ajoute les autres noeuds à la racine *)
    let rec pl2a_aux acl l map =
        match l with
        | [] -> ()
        | (phi, elems)::nextElems -> 
            add_acl acl phi elems map;
            pl2a_aux acl nextElems map
    in

    (* créé la racine *)
    match l with
    | [] -> ACL_Empty
    | (phi, elems)::nextElems ->
        let node = {fg = ACL_Empty; branches = (elems2node_list elems); fd = ACL_Empty} in
        Hashtbl.add map phi (ref node);

        let acl = ACL_Node(node) in
        pl2a_aux acl nextElems map;
        acl;;

(* renvoie la valeur du noeud précédent (dans l'abr) *)
let rec get_prev_val n abr =
    match abr with
    | Vide -> assert false
    | Noeud(l, k, r, p) ->
        if n < k then
            match l with
            | Vide -> assert false
            | Noeud(ll, lk, lr, rp) -> 
                if n = lk then k
                else get_prev_val n l

        else 
            match r with
            | Vide -> assert false
            | Noeud(rl, rk, rr, rp) -> 
                if n = rk then k
                else get_prev_val n r;;

(* renvoie le mot associé à la valeur v *)
let rec get_phi_from_val v abrp =
    match abrp with
    | Vide -> assert false (* v est forcément dans l'arbre *)
    | Noeud(l, k, r, phi) -> 
        if v = k then phi
        else if v < k then get_phi_from_val v l
        else get_phi_from_val v r;;

(* renvoie la branche qui contient la valeur v*)
let rec get_branch_from_branchList v branchList =
    match branchList with
    | [] -> assert false
    | branch::nextBranches -> 
        if List.hd branch.values = v then branch else get_branch_from_branchList v nextBranches;;

(* ajoute les transitions entre les noeuds concernés *)
let rec build_acl_links branchList branchNode abrp map id =

    (* ajoute la transition au noeud parent (fonction auxiliare pour factoriser le code) *)
    let babl_aux node prevNode branch prevBranch isLeft =
        match node with
        | ACL_Node(_) -> branch.values <- (List.hd branch.values)::(List.tl prevBranch.values)
        | ACL_Transition(t, r) -> branch.values <- (List.hd branch.values)::t::(List.tl prevBranch.values);
        | ACL_Empty -> 
            let newTransition = id#new_id in
            branch.values <- (List.hd branch.values)::newTransition::(List.tl branch.values);

            if isLeft then prevNode.fg <- ACL_Transition(newTransition, ref branchNode)
            else prevNode.fd <- ACL_Transition(newTransition, ref branchNode)            
    in

    match branchList with
    | [] -> ();
    | branch::nextBranches ->
        let valBranch = List.hd branch.values in
        let prevVal = get_prev_val valBranch abrp in
        let prevPhi = get_phi_from_val prevVal abrp in
        let prevNode = !(Hashtbl.find map prevPhi) in 
        let prevBranch = get_branch_from_branchList prevVal prevNode.branches in

        printf "Linking branch %d to branch %d\n" prevVal valBranch;

        if valBranch < prevVal then babl_aux prevNode.fg prevNode branch prevBranch true
        else babl_aux prevNode.fd prevNode branch prevBranch false;

        build_acl_links nextBranches branchNode abrp map id;;

(* créé l'ACL à partir de l'ABR avec mots et de la liste de mots *)
let create_acl abrp plist =
    let phiHash = Hashtbl.create tailleListe in
    let id = new identifier in
    let acl = phiList2acl plist phiHash in

    let rec ca_aux acl =
        match acl with
        | ACL_Empty
        | ACL_Transition(_) -> ()
        | ACL_Node(node) -> 
            printf "Building links on node %d\n" (List.hd (List.hd node.branches).values);
            build_acl_links (List.tl node.branches) node abrp phiHash id;
            ca_aux node.fg;
            ca_aux node.fd
    in

    ca_aux acl;
    acl;;

(* renvoie la branche correspondant aux transitions *)
let get_branch_by_transitions node transitions =
    let rec gbbt_aux branchList =
        match branchList with
        | [] -> assert false
        | branch::nextBranches -> 
            if (List.tl branch.values) = transitions then branch
            else gbbt_aux nextBranches
    in

    gbbt_aux node.branches;;

(* recherche dans un ACL *)
let rec acl_find acl v =
    let rec af_aux acl transitions =
        match acl with
        | ACL_Empty -> false
        | ACL_Transition(t, r) -> 
            let newTransitions = t:: transitions in
            let node = !r in
            let branch = get_branch_by_transitions node newTransitions in
            let valBranch = (List.hd branch.values) in


            if v = valBranch then true
            else if v < valBranch then af_aux node.fg newTransitions
            else af_aux node.fd newTransitions

        | ACL_Node(node) ->
            let branch = get_branch_by_transitions node transitions in
            let valBranch = (List.hd branch.values) in

            if v = valBranch then true
            else if v < valBranch then af_aux node.fg transitions
            else af_aux node.fd transitions
    in

    af_aux acl [];;

let l = gen_permutation tailleListe in

printf "Liste initiale :\n";
print_int_list l;

let abr = list2abr l in
let abrPhi = abr2abrPhi abr in

let phiList = parcours_comp_phi abrPhi in 

printf "\nphiList :\n";
print_phiList phiList;

let acl = create_acl abrPhi phiList in
let valToSearch = 7 in

if (acl_find acl valToSearch) then printf "\nLa valeur %d est dans l'ACL.\n" valToSearch
else printf "\nLa valeur %d n'est pas dans l'ACL.\n" valToSearch;

exit 0;;