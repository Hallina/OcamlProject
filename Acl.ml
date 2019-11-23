open Abr_phi

type  abr_compressed_list = 
    | ACL_Empty
    | ACL_Transition of int * acl_node ref
    | ACL_Node of acl_node 
        and acl_node = {mutable fg: abr_compressed_list; branches: acl_branch list; mutable fd: abr_compressed_list}
        and acl_branch = {mutable values: int list}

(* générateur d'identifiant *)
class identifier =
    object
      val mutable id = 0
      method get_id = id
      method new_id = id <- id + 1; id
    end

(* convertit une liste d'entiers en liste de branches *)
let elems2node_list elems =
    let rec e2ell_aux elems el =
        match elems with
        | [] -> el
        | h::t -> e2ell_aux t ({values = [h]}::el)
    in

    List.rev (e2ell_aux elems [])

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
        else aa_aux node.fd node false

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
        acl

(* fonctions de test du contenu d'un ACL ou d'un ABRP *)
let is_abrp_node abrp = match abrp with | ABRP_Node(_) -> true | _ -> false
let is_abrp_empty abrp = match abrp with ABRP_Empty -> true | _ -> false
let is_acl_node acl = match acl with ACL_Node(_) -> true | _ -> false
let is_acl_empty acl = match acl with ACL_Empty -> true | _ -> false
let is_acl_transition acl = match acl with ACL_Transition(_) -> true | _ -> false

(* fonctions renvoyant la clé et le mot de l'ABRP *)
let get_abrp_key abrp = match abrp with ABRP_Node(fg, k, fd, p) -> k | _ -> assert false
let get_abrp_phi abrp = match abrp with ABRP_Node(fg, k, fd, phi) -> phi | _ -> assert false

(* fonctions renvoyant le noeud de l'ACL correspondant aux transitions et le noeud de l'acl *)
let get_node_from_transition acl = match acl with ACL_Transition(t, r) -> !r, t | _ -> assert false
let get_acl_node acl = match acl with ACL_Node(node) -> node | _ -> assert false

(* renvoie la branch correspondant à la valeur *)
let rec get_branch_from_val branches value =
    match branches with
    | [] -> assert false
    | branch::nextBranches -> 
        if (List.hd branch.values = value) then branch else get_branch_from_val nextBranches value

(* met à jour les transitions d'une branche *)
let rec update_trans branches value transitions =
    match branches with
    | [] -> assert false
    | branch::nextBranches ->
        if List.hd branch.values = value then branch.values <- (List.hd branch.values)::transitions
        else update_trans nextBranches value transitions

(* créé une transition entre deux noeuds de l'ACL *)
let make_transition acl abrp valNode map id leftSon =
    let newTransition = id#new_id in
    let nodeToGo = Hashtbl.find map (get_abrp_phi abrp) in
    let curTransitions = List.tl (get_branch_from_val acl.branches valNode).values in

    if leftSon then acl.fg <- ACL_Transition(newTransition, nodeToGo)
    else acl.fd <- ACL_Transition(newTransition, nodeToGo);

    update_trans (!nodeToGo).branches (get_abrp_key abrp) (newTransition::curTransitions);
    !nodeToGo

(* construit les transitions de l'ACL *)
let rec build_acl_links acl abrp map id fromTransition =
    match acl with
    | ACL_Node(aclNode) -> (
        match abrp with
        | ABRP_Node(fg, k, fd, p) ->
            (* fils gauche *)
            if is_acl_node aclNode.fg && is_abrp_node fg then (
                if fromTransition then
                    update_trans (get_acl_node aclNode.fg).branches (get_abrp_key fg) (List.tl (get_branch_from_val aclNode.branches k).values);

                build_acl_links aclNode.fg fg map id fromTransition)

            else if is_acl_empty aclNode.fg && is_abrp_node fg then (
                let fgAclNode = make_transition aclNode fg k map id true in
                build_acl_links (ACL_Node fgAclNode) fg map id true)


            else if is_acl_transition aclNode.fg && is_abrp_node fg then (
                let trans = get_node_from_transition aclNode.fg in

                update_trans (fst trans).branches (get_abrp_key fg) ((snd trans)::(List.tl (get_branch_from_val aclNode.branches k).values));

                build_acl_links (ACL_Node (fst trans)) fg map id true)

            else if is_acl_empty aclNode.fg && is_abrp_empty fg then ();

            (* fils droit *)
            if is_acl_node aclNode.fd && is_abrp_node fd then (
                if fromTransition then
                    update_trans (get_acl_node aclNode.fd).branches (get_abrp_key fd) (List.tl (get_branch_from_val aclNode.branches k).values);

                build_acl_links aclNode.fd fd map id fromTransition)

            else if is_acl_empty aclNode.fd && is_abrp_node fd then (
                let fdAclNode = make_transition aclNode fd k map id false in
                build_acl_links (ACL_Node fdAclNode) fd map id true)

            else if is_acl_transition aclNode.fd && is_abrp_node fd then (
                let trans = get_node_from_transition aclNode.fd in
                update_trans (fst trans).branches (get_abrp_key fd) ((snd trans)::(List.tl (get_branch_from_val aclNode.branches k).values));
                build_acl_links (ACL_Node (fst trans)) fd map id true)

            else if is_acl_empty aclNode.fd && is_abrp_empty fd then ()
        | _ -> assert false )

    | _ -> assert false

(* créé l'ACL à partir de l'ABR avec mots et de la liste de mots *)
let create_acl abrp plist tailleListe =
    let phiHash = Hashtbl.create tailleListe in
    let id = new identifier in
    let acl = phiList2acl plist phiHash in

    build_acl_links acl abrp phiHash id false;
    acl

(* renvoie la branche correspondant aux transitions *)
let get_branch_by_transitions node transitions =
    let rec gbbt_aux branchList =
        match branchList with
        | [] -> assert false
        | branch::nextBranches -> 
            if (List.tl branch.values) = transitions then branch
            else gbbt_aux nextBranches
    in

    gbbt_aux node.branches


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

    af_aux acl []

(* renvoie le nombre de noeuds et de clés*)
let acl_stats acl =
    let nbKeys = ref 0 in
    let nbNodes = ref 0 in
    let deepness = ref 0 in

    let rec as_aux acl curDeepness =
        match acl with
        | ACL_Empty -> ()
        | ACL_Transition(_) -> ()
        | ACL_Node(node) ->
            if curDeepness > !deepness then deepness := curDeepness;

            nbKeys := (!nbKeys + (List.length node.branches));
            nbNodes := (!nbNodes + 1);
            as_aux node.fg (curDeepness + 1);
            as_aux node.fd (curDeepness + 1);
    in

    as_aux acl 1;
    (!nbNodes, !nbKeys)