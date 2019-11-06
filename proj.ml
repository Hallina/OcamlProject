open Printf;;

Random.self_init();;

let tailleListe = 15;;

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
let rec get_pred_val n abr =
    match abr with
    | Vide -> assert false
    | Noeud(l, k, r, p) ->
        if n < k then
            match l with
            | Vide -> assert false
            | Noeud(ll, lk, lr, rp) -> 
                if n = lk then k
                else get_pred_val n l

        else 
            match r with
                | Vide -> assert false
                | Noeud(rl, rk, rr, rp) -> 
                    if n = rk then k
                    else get_pred_val n r;;

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
let rec build_acl_links node branchNode abrp map id =
    match node with
    | [] -> ();
    | ({fg; lbl; fd} as branch)::t ->
        let valBranch = List.hd lbl in
        let predVal = get_pred_val (List.hd lbl) abrp in
        let predPhi = get_phi_from_val predVal abrp in
        let predNode = !(Hashtbl.find map predPhi) in 

        printf "Linking node %d to node containing %d.\n" predVal valBranch;

        let branchPred = get_branch_from_node predVal predNode in
        let newTransition = id#get_id in

        if valBranch < predVal then branchPred.fg <- ACL_Transition(newTransition, ref branchNode)
        else branchPred.fd <- ACL_Transition(newTransition, ref branchNode);

        add_transitions_to_branch branch (List.tl branchPred.lbl) newTransition;
        build_acl_links t branchNode abrp map id;
    ;;

(*  construit l'arbre compressé qui utilise des listes pour les transitions *)
let create_acl abrPhi phiList =
    let phiHash = Hashtbl.create tailleListe in
    let id = new identifier in
    let acl = phiList2acl phiList phiHash in

    let rec ab_aux acl =
        match acl with
        | ACL_Node([]) -> assert false
        | ACL_Transition(_)
        | ACL_Empty -> ()
        | ACL_Node({fg; lbl; fd}::nextBranch) ->
            build_acl_links nextBranch acl abrPhi phiHash id;
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

        | ACL_Node(({fg; lbl; fd})::_) ->
            let valBranch = List.hd lbl in

            if v = valBranch then acl
            else if v < valBranch then as_aux fg (List.tl lbl)
            else as_aux fd (List.tl lbl);
    in

    as_aux acl [];;

type  abr_compressed_map = 
    | ACM_Empty
    | ACM_Transition of int * abr_compressed_map ref
    | ACM_Node of nodeACM

    and nodeACM = {map: ((string, branchACM) Hashtbl.t)}
    and branchACM = {mutable fg: abr_compressed_map; value: int; mutable fd: abr_compressed_map};;


(* convertit les branches d'un noeud d'ACL vers un noeud ACM *)
let rec acl_node2acm_node aclNode map =
    match aclNode with
    | [] -> ()
    | {lbl; _}::t -> 
        let key = (int_list2string (List.tl lbl)) in
        let branch = {fg = ACM_Empty; value = (List.hd lbl); fd = ACM_Empty} in

        Hashtbl.add map key branch;
        acl_node2acm_node t map;;

(* convertit un ACL vers un ACM incomplet (sans les transitions) *)
let rec acl2acm aclNode abrp phiHash =
    match aclNode with
    | ACL_Node([]) -> assert false
    | ACL_Transition(_) | ACL_Empty -> ACM_Empty
    | ACL_Node((aclNode::nextAclNode) as nodeList) -> 
        let vl = List.hd aclNode.lbl in
        let node = {map = Hashtbl.create (List.length nodeList)} in
        let branch = {fg = acl2acm aclNode.fg abrp phiHash; value = vl; fd = acl2acm aclNode.fd abrp phiHash} in
        let key = int_list2string (List.tl aclNode.lbl) in

        Hashtbl.add node.map key branch;
        acl_node2acm_node nextAclNode node.map;

        let res = ACM_Node(node) in
        let phiKey = get_phi_from_val vl abrp in

        Hashtbl.add phiHash phiKey (ref res);

        res;;

(* renvoie la première branche du noeud d'un ACL *)
let pop_acl_node acl =
    match acl with
    | ACL_Empty | ACL_Transition(_) | ACL_Node([]) -> assert false
    | ACL_Node(branch::nextBranch) -> branch;;

(* renvoie la première branche du noeud d'un ACM *)
let pop_acm_node acm =
    match acm with
    | ACM_Empty | ACM_Transition(_) -> assert false
    | ACM_Node({map}) -> 
        Hashtbl.find map (int_list2string []);;

(* créé les transitions d'un ACM à partir d'un ACL *)
let rec update_transitions branchList acm abrp phiHash =
    let ut_aux acl lbl isLeft =
        match acl with
        | ACL_Empty | ACL_Node(_) -> ()
        | ACL_Transition(t, r) -> 
            let acmBranch = Hashtbl.find acm.map (int_list2string (List.tl lbl)) in
            let predPhi = get_phi_from_val (List.hd (pop_acl_node !r).lbl) abrp in
            let transition = ACM_Transition(t, (Hashtbl.find phiHash predPhi)) in

            if isLeft then acmBranch.fg <- transition
            else acmBranch.fd <- transition;
    in

    match branchList with
    | [] -> ()
    | {fg; lbl; fd}::nextBranch -> 
        match fg with
        | ACL_Empty | ACL_Node(_) -> ()
        | ACL_Transition(t, r) -> 
            ut_aux fg lbl true;
            update_transitions nextBranch acm abrp phiHash;

        match fd with
        | ACL_Empty | ACL_Node(_) -> ()
        | ACL_Transition(t, r) -> 
            ut_aux fd lbl false;
            update_transitions nextBranch acm abrp phiHash;;

(* construit l'arbre compressé qui utilise une map pour les transitions *)
let create_acm acl abrp abr =
    let phiHash = Hashtbl.create tailleListe in
    let acm = acl2acm acl abrp phiHash in

    let rec ca_aux acl acm =
        match acl with
        | ACL_Node([]) -> assert false
        | ACL_Transition(_)
        | ACL_Empty -> ()
        | ACL_Node(({fg; lbl; fd}::nextBranch) as branchList) ->
            match acm with
            | ACM_Empty -> assert false
            | ACM_Transition(_) -> ()
            | ACM_Node(node) ->
                let acmBranch = pop_acm_node acm in

                update_transitions branchList node abrp phiHash;

                ca_aux fg acmBranch.fg;
                ca_aux fd acmBranch.fd;
    in

    ca_aux acl acm;;

(* let l = gen_permutation 10 in *)
let l = [2;5;4;10;6;7;1;9;3;8] in 

printf "Liste initiale :\n";
print_int_list l;

let abr = list2abr l in
let abrPhi = abr2abrPhi abr in

let phiList = parcours_comp_phi abrPhi in 

printf "phiList :\n";
print_phiList phiList;

let acl = create_acl abrPhi phiList in
let valToSearch = 8 in
let node = acl_search valToSearch acl in

if node != ACL_Empty then printf "La valeur %d se trouve dans l'ACL.\n" valToSearch
else printf "La valeur %d ne se trouve pas dans l'ACL.\n" valToSearch;

let acm = create_acm acl abrPhi abr in

exit 0;;