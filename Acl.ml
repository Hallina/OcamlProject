open Abr_phi;;
open Printf;;

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

let test2 id =
    ();;

let test () =
    let id = new identifier in
    test2 id;;


(* renvoie la branche qui contient la valeur v*)
let rec get_branch_from_branchList v branchList =
    test ();
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
let create_acl abrp plist tailleListe =
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