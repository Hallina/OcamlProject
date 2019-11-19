open Printf;;
open Gen;;
open Abr_phi;;
open Acl;;

type  abr_compressed_hashmap = 
    | ACH_Empty
    | ACH_Transition of int * ach_node ref
    | ACH_Node of ach_node
        and ach_node = {mutable fg: abr_compressed_hashmap; hmap: ((string, int) Hashtbl.t); mutable fd: abr_compressed_hashmap};;

(* convertit les branches d'un noeud d'ACL vers un noeud d'ACH *)
let acl_node2ach_node aclNode hmap =
    let rec an2am_aux branchList =
        match branchList with
        | [] -> (); printf "\n";
        | branch::nextBranches ->
            let key = int_list2string (List.tl branch.values) in
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

            let value = List.hd (List.hd aclNode.branches).values in
            let phiKey = get_phi_from_val value abrp in

            printf "Node %d : " value;

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
            let value = List.hd (List.hd (!r).branches).values in
            let phi = get_phi_from_val value abrp in
            let node = Hashtbl.find pmap phi in

            if isLeft then achParent.fg <- ACH_Transition(t, node)
            else achParent.fd <- ACH_Transition(t, node);

            printf "Adding transition %d to node %d.\n" t value;
        | _, _ -> ()
    in

    match acl, ach with
    | ACL_Node(aclNode), ACH_Node(achNode) ->
        ba_aux aclNode.fg aclNode achNode.fg achNode true;
        ba_aux aclNode.fd aclNode achNode.fd achNode false;
    | _, _ -> ();;

(* construit l'arbre compressé qui utilise une hashmap pour les transitions *)
let create_ach acl abrp tailleListe =
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