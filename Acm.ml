open Printf;;
open Gen;;
open Abr_phi;;
open Acl;;

module StrMap = Map.Make(String);;

type  abr_compressed_map = 
    | ACM_Empty
    | ACM_Transition of int * acm_node ref
    | ACM_Node of acm_node
        and acm_node = {mutable fg: abr_compressed_map; map: int StrMap.t; mutable fd: abr_compressed_map};;


(* convertit les branches d'un noeud d'ACL vers un noeud d'ACM *)
let rec acl_node2acm_node branchList map =
    match branchList with
    | [] -> printf "\n"; map
    | branch::nextBranches ->
        let key = int_list2string (List.tl branch.values) in
        let value = List.hd branch.values in

        printf "\"%s\" -> %d; " key value;

        acl_node2acm_node nextBranches (StrMap.add key value map);;

(* convertit un ACL vers un ACM incomplet (sans les transitions) *)
let acl2acm aclNode abrp pmap =
    let rec a2a_aux aclNode =
        match aclNode with
        | ACL_Transition(_) | ACL_Empty -> ACM_Empty
        | ACL_Node(aclNode) ->
            let acmNode = {
                fg = a2a_aux aclNode.fg; 
                map = acl_node2acm_node aclNode.branches StrMap.empty;
                fd = a2a_aux aclNode.fd} in

            let value = List.hd (List.hd aclNode.branches).values in
            let phiKey = get_phi_from_val value abrp in

            printf "Node %d : " value;

            Hashtbl.add pmap phiKey (ref acmNode);

            ACM_Node(acmNode)
    in

    a2a_aux aclNode;;

(* complète l'ACM en ajoutant les transitions à partir de l'ACL *)
let rec build_acm acl acm abrp pmap =

    (* met à jour les transitions du parent (fonction auxiliaire pour factoriser le code) *)
    let rec ba_aux aclSon aclParent acmSon acmParent isLeft =
        match aclSon, acmSon with
        | ACL_Node(_), ACM_Node(_) -> build_acm aclSon acmSon abrp pmap;
        | ACL_Transition(t, r), ACM_Empty ->
            let value = List.hd (List.hd (!r).branches).values in
            let phi = get_phi_from_val value abrp in
            let node = Hashtbl.find pmap phi in

            if isLeft then acmParent.fg <- ACM_Transition(t, node)
            else acmParent.fd <- ACM_Transition(t, node);

            printf "Adding transition %d to node %d.\n" t value;
        | _, _ -> ()
    in

    match acl, acm with
    | ACL_Node(aclNode), ACM_Node(acmNode) ->
        ba_aux aclNode.fg aclNode acmNode.fg acmNode true;
        ba_aux aclNode.fd aclNode acmNode.fd acmNode false;
    | _, _ -> ();;


(* construit l'arbre compressé qui utilise une map pour les transitions *)
let create_acm acl abrp tailleListe =
    let phiHash = Hashtbl.create tailleListe in
    let acm = acl2acm acl abrp phiHash in

    build_acm acl acm abrp phiHash;
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