open Printf;;

Random.self_init();;

let tailleListe = 100;;

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

type abr = ABR_Empty | ABR_Node of (abr * int * abr);;

(*affichage d'un abr (parcours infixe)*)
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

(*ajout d'une feuille dans un arbre (Q1.3)*)
let rec add n abr = 
    match abr with
    | ABR_Empty -> ABR_Node(ABR_Empty, n, ABR_Empty)
    | ABR_Node(left, k, right) -> 
        if n = k then abr
        else if n < k then ABR_Node((add n left), k, right)
        else ABR_Node(left, k, (add n right));;

(*question 1.3*)
let list2abr l =
    let rec l2a_aux l abr = 
        match l with
        | [] -> abr
        | h::t -> l2a_aux t (add h abr)
    in

    l2a_aux l ABR_Empty;;


(*2 - Compression des ABR*)

type 'a abr_phi = ABRP_Empty | ABRP_Node of ('a abr_phi * 'a * 'a abr_phi * string);;

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
    | ACL_Transition of int * acl_node ref
    | ACL_Node of acl_node 
        and acl_node = {mutable fg: abr_compressed_list; branches: acl_branch list; mutable fd: abr_compressed_list}
        and acl_branch = {mutable values: int list};;

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
        | (phi, elems)::nextABR_Emptylems -> 
            add_acl acl phi elems map;
            pl2a_aux acl nextABR_Emptylems map
    in

    (* créé la racine *)
    match l with
    | [] -> ACL_Empty
    | (phi, elems)::nextABR_Emptylems ->
        let node = {fg = ACL_Empty; branches = (elems2node_list elems); fd = ACL_Empty} in
        Hashtbl.add map phi (ref node);

        let acl = ACL_Node(node) in
        pl2a_aux acl nextABR_Emptylems map;
        acl;;

(* renvoie la valeur du noeud précédent (dans l'abr) *)
let rec get_prev_val_and_phi n abr =
    match abr with
    | ABRP_Empty -> assert false
    | ABRP_Node(l, k, r, p) ->
        if n < k then
            match l with
            | ABRP_Empty -> assert false
            | ABRP_Node(ll, lk, lr, rp) -> 
                if n = lk then (k, p)
                else get_prev_val_and_phi n l

        else 
            match r with
            | ABRP_Empty -> assert false
            | ABRP_Node(rl, rk, rr, rp) -> 
                if n = rk then (k, p)
                else get_prev_val_and_phi n r;;

(* renvoie le mot associé à la valeur v *)
let rec get_phi_from_val v abrp =
    match abrp with
    | ABRP_Empty -> assert false (* v est forcément dans l'arbre *)
    | ABRP_Node(l, k, r, phi) -> 
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
        let prevValPhi = get_prev_val_and_phi valBranch abrp in
        let prevVal = fst prevValPhi in
        let prevPhi = snd prevValPhi in
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
            af_aux (ACL_Node(!r)) (t::transitions)

        | ACL_Node(node) ->
            let branch = get_branch_by_transitions node transitions in
            let valBranch = (List.hd branch.values) in

            if v = valBranch then true
            else if v < valBranch then af_aux node.fg transitions
            else af_aux node.fd transitions
    in

    af_aux acl [];;

type  abr_compressed_hashmap = 
    | ACH_Empty
    | ACH_Transition of int * ach_node ref
    | ACH_Node of ach_node
        and ach_node = {mutable fg: abr_compressed_hashmap; hmap: ((string, int) Hashtbl.t); mutable fd: abr_compressed_hashmap};;

(* convertit les branches d'un noeud d'ACL vers un noeud ACH *)
let acl_node2ach_node aclNode hmap =
    let rec an2am_aux branchList =
        match branchList with
        | [] -> (); printf "\n";
        | branch::nextBranches ->
            let key = (int_list2string (List.tl branch.values)) in
            let value = List.hd branch.values in

            printf "\"%s\" -> %d; " key value;

            Hashtbl.add hmap key value;
            an2am_aux nextBranches
    in

    an2am_aux aclNode.branches;;
        
(* convertit un ACL vers un ACH incomplet (sans les transitions) *)
let acl2ach aclNode abrp pmap =
    let rec a2a_aux aclNode =
        match aclNode with
        | ACL_Transition(_) | ACL_Empty -> ACH_Empty
        | ACL_Node(aclNode) ->
            let achNode = {
                fg = a2a_aux aclNode.fg; 
                hmap = Hashtbl.create (List.length aclNode.branches);
                fd = a2a_aux aclNode.fd} in

            let vl = List.hd (List.hd aclNode.branches).values in
            let phiKey = get_phi_from_val vl abrp in

            printf "Node %d : " vl;

            acl_node2ach_node aclNode achNode.hmap;      

            Hashtbl.add pmap phiKey (ref achNode);

            ACH_Node(achNode)
    in

    a2a_aux aclNode;;

(* complète l'ACH en ajoutant les transitions à partir de l'ACL *)
let rec build_ach acl ach abrp pmap =

    (* met à jour les transitions du parent (fonction auxiliaire pour factoriser le code) *)
    let rec ba_aux aclSon aclParent achSon achParent isLeft =
        match aclSon, achSon with
        | ACL_Node(_), ACH_Node(_) -> build_ach aclSon achSon abrp pmap;
        | ACL_Transition(t, r), ACH_Empty ->
            let vl = List.hd (List.hd (!r).branches).values in
            let phi = get_phi_from_val vl abrp in
            let node = Hashtbl.find pmap phi in

            if isLeft then achParent.fg <- ACH_Transition(t, node)
            else achParent.fd <- ACH_Transition(t, node);

            printf "Adding transition %d to node %d.\n" t vl;
        | _, _ -> ()
    in

    match acl, ach with
    | ACL_Node(aclNode), ACH_Node(achNode) ->
        ba_aux aclNode.fg aclNode achNode.fg achNode true;
        ba_aux aclNode.fd aclNode achNode.fd achNode false;
    | _, _ -> ();;

(* construit l'arbre compressé qui utilise une hashmap pour les transitions *)
let create_ach acl abrp =
    let phiHash = Hashtbl.create tailleListe in
    let ach = acl2ach acl abrp phiHash in

    build_ach acl ach abrp phiHash;
    ach;;

(* recherche dans un ACH *)
let rec ach_find ach v =
    let rec af_aux ach transitions =
        match ach with
        | ACH_Empty -> false
        | ACH_Transition(t, r) -> 
            af_aux (ACH_Node(!r)) ((string_of_int t) ^ ";" ^ transitions)

        | ACH_Node(node) ->
            let valBranch = Hashtbl.find node.hmap transitions in

            if v = valBranch then true
            else if v < valBranch then af_aux node.fg transitions
            else af_aux node.fd transitions
    in

    af_aux ach "";;


module StrMap = Map.Make(String);;

type  abr_compressed_map = 
    | ACM_Empty
    | ACM_Transition of int * acm_node ref
    | ACM_Node of acm_node
        and acm_node = {mutable fg: abr_compressed_map; map: int StrMap.t; mutable fd: abr_compressed_map};;


(* fonction passée à Hastbl.fold pour créer une map à partir d'une hashmap *)
let rec hash2map key value map = 
    StrMap.add key value map;;
        
(* convertit un ACH vers un ACM incomplet (sans les transitions) *)
let ach2acm achNode abrp pmap =
    let rec a2a_aux achNode =
        match achNode with
        | ACH_Transition(_) | ACH_Empty -> ACM_Empty
        | ACH_Node(achNode) ->
            let acmNode = {
                fg = a2a_aux achNode.fg; 
                map = Hashtbl.fold hash2map achNode.hmap StrMap.empty;
                fd = a2a_aux achNode.fd} in

            let value = Hashtbl.find achNode.hmap "" in
            let phiKey = get_phi_from_val value abrp in

            printf "Node %d : " value;

            Hashtbl.add pmap phiKey (ref acmNode);

            ACM_Node(acmNode)
    in

    a2a_aux achNode;;

(* complète l'ACH en ajoutant les transitions à partir de l'ACL *)
let rec build_acm ach acm abrp pmap =

    (* met à jour les transitions du parent (fonction auxiliaire pour factoriser le code) *)
    let rec ba_aux achSon achParent acmSon acmParent isLeft =
        match achSon, acmSon with
        | ACH_Node(_), ACM_Node(_) -> build_acm achSon acmSon abrp pmap;
        | ACH_Transition(t, r), ACM_Empty ->
            let value = Hashtbl.find (!r).hmap "" in
            let phi = get_phi_from_val value abrp in
            let node = Hashtbl.find pmap phi in

            if isLeft then acmParent.fg <- ACM_Transition(t, node)
            else acmParent.fd <- ACM_Transition(t, node);

            printf "Adding transition %d to node %d.\n" t value;
        | _, _ -> ()
    in

    match ach, acm with
    | ACH_Node(achNode), ACM_Node(acmNode) ->
        ba_aux achNode.fg achNode acmNode.fg acmNode true;
        ba_aux achNode.fd achNode acmNode.fd acmNode false;
    | _, _ -> ();;

(* construit l'arbre compressé qui utilise une map pour les transitions *)
let create_acm ach abrp =
    let phiHash = Hashtbl.create tailleListe in
    let acm = ach2acm ach abrp phiHash in

    build_acm ach acm abrp phiHash;
    acm;;

(* recherche dans un AcM *)
let rec acm_find acm v =
    let rec af_aux acm transitions =
        match acm with
        | ACM_Empty -> false
        | ACM_Transition(t, r) -> 
            af_aux (ACM_Node(!r)) ((string_of_int t) ^ ";" ^ transitions)

        | ACM_Node(node) ->
            let valBranch = StrMap.find transitions node.map in

            if v = valBranch then true
            else if v < valBranch then af_aux node.fg transitions
            else af_aux node.fd transitions
    in

    af_aux acm "";;

let valToSearch = 7 in
let l = gen_permutation tailleListe in

printf "Liste initiale :\n";
print_int_list l;

let abr = list2abr l in
let abr_mem = Gc.allocated_bytes () in

let abrPhi = abr2abrPhi abr in
let abrPhi_mem = Gc.allocated_bytes () -. abr_mem in

let phiList = parcours_comp_phi abrPhi in 
let phiList_mem = Gc.allocated_bytes () -. abrPhi_mem in

printf "\nphiList :\n";
print_phiList phiList;

let acl = create_acl abrPhi phiList in
let acl_mem = Gc.allocated_bytes () -. phiList_mem in

if (acl_find acl valToSearch) then printf "\nLa valeur %d est dans l'ACL.\n" valToSearch
else printf "\nLa valeur %d n'est pas dans l'ACL.\n" valToSearch;

let ach = create_ach acl abrPhi in
let ach_mem = Gc.allocated_bytes () -. acl_mem in

if (ach_find ach valToSearch) then printf "\nLa valeur %d est dans l'ACH.\n" valToSearch
else printf "\nLa valeur %d n'est pas dans l'ACH.\n" valToSearch;

let acm = create_acm ach abrPhi in
let acm_mem = Gc.allocated_bytes () -. ach_mem in

if (acm_find acm valToSearch) then printf "\nLa valeur %d est dans l'ACM.\n" valToSearch
else printf "\nLa valeur %d n'est pas dans l'ACM.\n" valToSearch;

(* premiers tests, pas très concluants... *)
printf "\n\nMémoire utilisée pour l'ABR :     %F\n" abr_mem;
printf "Mémoire utilisée pour l'ABR Phi : %F\n" abrPhi_mem;
printf "Mémoire utilisée pour phiList :   %F\n" phiList_mem;
printf "Mémoire utilisée pour l'ACL :     %F\n" acl_mem;
printf "Mémoire utilisée pour l'ACH :     %F\n" ach_mem;
printf "Mémoire utilisée pour l'ACM :     %F\n" acm_mem;

exit 0;;