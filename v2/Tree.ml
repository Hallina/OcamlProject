module Utils =
    struct
        open Printf

        let print_int_list l = 
            List.iter (fun x -> print_int x; print_char ';') l;
            print_char '\n'

        (*suppresion du nième élément d'une liste*)
        let rec remove_at n l = 
            match l with
            | [] -> []
            | h::t -> if n = 0 then t else h::remove_at (n-1) t

        (*question 1.1*)
        let exctraction_alea l p = 
            let pos = Random.int (List.length l) in
            let elem = List.nth l pos in

            ((remove_at pos l), elem::p)

        (*initialisation d'une liste triée de 1 à n (Q1.2)*)
        let rec init_list l n =
            if n = 1 then n::l else init_list (n::l) (n-1)

        (*vidage de L dans P (Q1.2)*)
        let rec vidage l p acc =
            let res = exctraction_alea l p in
            if acc = 1 then snd res else vidage (fst res) (snd res) (acc-1)

        (*question 1.2*)
        let gen_permutation n =
            Random.self_init();
            vidage (init_list [] n) [] n

        (* transforme une liste d'entier en string *)
        let int_list2string l = 
            let rec il2s_aux l acc =
                match l with 
                | [] -> acc
                | h::t -> 
                    il2s_aux t (acc ^ (string_of_int h) ^ ";")
            in

            il2s_aux l ""

            (* convertit un jeu de test en liste d'entiers *)
            let get_test_list path =
                let ic = open_in path in
                let contents = input_line ic in

                close_in ic;
                
                let lexer = Genlex.make_lexer ["," ; "[" ; "]"] in
                let stream = Stream.of_string contents in
                let tokenStream = lexer stream in

                let resList = ref [] in
                let resLen = ref 0 in

              try
                while true do
                  match Stream.next tokenStream with 
                        | Genlex.Int value -> resList := value::!resList; resLen := !resLen + 1
                        | Genlex.Kwd value -> ()
                        | _ -> failwith "Expected int."
                done;
                assert false
              with Stream.Failure -> (List.rev !resList), !resLen

        let table =
            [|98;  6; 85;150; 36; 23;112;164;135;207;169;  5; 26; 64;165;219;
            61; 20; 68; 89;130; 63; 52;102; 24;229;132;245; 80;216;195;115;
            90;168;156;203;177;120;  2;190;188;  7;100;185;174;243;162; 10;
            237; 18;253;225;  8;208;172;244;255;126;101; 79;145;235;228;121;
            123;251; 67;250;161;  0;107; 97;241;111;181; 82;249; 33; 69; 55;
            59;153; 29;  9;213;167; 84; 93; 30; 46; 94; 75;151;114; 73;222;
            197; 96;210; 45; 16;227;248;202; 51;152;252;125; 81;206;215;186;
            39;158;178;187;131;136;  1; 49; 50; 17;141; 91; 47;129; 60; 99;
            154; 35; 86;171;105; 34; 38;200;147; 58; 77;118;173;246; 76;254;
            133;232;196;144;198;124; 53;  4;108; 74;223;234;134;230;157;139;
            189;205;199;128;176; 19;211;236;127;192;231; 70;233; 88;146; 44;
            183;201; 22; 83; 13;214;116;109;159; 32; 95;226;140;220; 57; 12;
            221; 31;209;182;143; 92;149;184;148; 62;113; 65; 37; 27;106;166;
            3; 14;204; 72; 21; 41; 56; 66; 28;193; 40;217; 25; 54;179;117;
            238; 87;240;155;180;170;242;212;191;163; 78;218;137;194;175;110;
            43;119;224; 71;122;142; 42;160;104; 48;247;103; 15; 11;138;239|]

        let key_hash key = 
            let len = String.length key in
            let hh = Array.make 2 0 in

            if key = "" then (hh.(0) <- table.(0); hh.(1) <- table.(1))
            else begin

                for j = 0 to 1 do 
                    let h = ref table.((Char.code (String.get key 0))+ j mod 256) in

                    for i = 1 to (len - 1) do
                        h := table.(!h lxor (Char.code (String.get key i)))
                    done; 

                    hh.(j) <- !h;
                done;
            end;
            
            let h1 = if hh.(0) < 16 then "0" ^ (sprintf "%X" hh.(0)) else (sprintf "%X" hh.(0)) in
            let h2 = if hh.(1) < 16 then "0" ^ (sprintf "%X" hh.(1)) else (sprintf "%X" hh.(1)) in

            int_of_string ("0x" ^ h1 ^ h2);;
    end

module Abr =
    struct
        type abr = ABR_Empty | ABR_Node of (abr * int * abr)

        (* affichage d'un abr (parcours infixe) *)
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
            print_char '\n'
         
        (* ajout d'une feuille dans un arbre (Q1.3) *)
        let rec add n abr = 
            match abr with
            | ABR_Empty -> ABR_Node(ABR_Empty, n, ABR_Empty)
            | ABR_Node(left, k, right) -> 
                if n = k then abr
                else if n < k then ABR_Node((add n left), k, right)
                else ABR_Node(left, k, (add n right))

        (* question 1.3 *)
        let list2abr l =
            let rec l2a_aux l abr = 
                match l with
                | [] -> abr
                | h::t -> l2a_aux t (add h abr)
            in

            l2a_aux l ABR_Empty

        (* recherche dans un ABR *)
        let rec abr_find abr n =
            match abr with
            | ABR_Empty -> false
            | ABR_Node(fg, v, fd) ->
                if n = v then true
                else if n < v then abr_find fg n
                else abr_find fd n

        (* renvoie le nombre de noeuds, de clés et la valeur la plus profonde de l'arbre *)
        let abr_stats abr =
            let nbKeys = ref 0 in
            let nbNodes = ref 0 in
            let deepness = ref 0 in
            let deepestVal = ref 0 in

            let rec as_aux abr curDeepness =
                match abr with
                | ABR_Empty -> ()
                | ABR_Node(fg, key, fd) ->
                    if curDeepness > !deepness then deepness := curDeepness;
                    if curDeepness = !deepness then deepestVal := key;

                    nbKeys := (!nbKeys + 1);
                    nbNodes := (!nbNodes + 1);
                    as_aux fg (curDeepness + 1);
                    as_aux fd (curDeepness + 1);
            in

            as_aux abr 1;
            [!nbNodes; !nbKeys; !deepestVal]
    end

module Abr_phi =
    struct 
        open Utils
        open Abr

        type abr_phi = ABRP_Empty | ABRP_Node of (abr_phi * int * abr_phi * string)

        let print_abrPhi abr =
            let rec pap_aux abr =
                match abr with
                | ABRP_Empty -> ()
                | ABRP_Node(left, k, right, phi) -> 
                    pap_aux left;
                    Printf.printf "%d, %s; " k phi;
                    pap_aux right
                in

            pap_aux abr;
            print_char '\n'

        (* question 2.4 *)
        let rec phi abr =
            match abr with
            | ABR_Empty -> ""
            | ABR_Node(left, k, right) -> "(" ^ (phi left) ^ ")" ^ (phi right)


        (* créé un arbre dont les noeuds contiennent le mot associé aux sous-arbre *)
        let rec abr2abrPhi abr =
            match abr with
            | ABR_Empty -> ABRP_Empty
            | ABR_Node(left, k, right) -> ABRP_Node((abr2abrPhi left), k, (abr2abrPhi right), (phi abr))

        (* affiche la liste de mots l *)
        let rec print_phiList l =
            match l with
            | [] -> print_char '\n'
            | (phi, elems)::t -> print_string phi; print_int_list elems; Printf.printf "\n"; print_phiList t

        (* renvoie les valeurs correspondant au mot p *)
        let rec get_vals_of_phi l p =
            match l with
            | [] -> []
            | (phi, elems)::t -> if phi = p then elems else get_vals_of_phi t p

        (* teste l'appartenance d'un mot à une liste *)
        let rec not_contains phi l =
            match l with
            | [] -> true
            | (p, el)::t -> if p = phi then false else not_contains phi t

        (* renvoie la liste des mots de l2 que l1 ne contient pas *)
        let rec get_different_phis l1 l2 =
            match l2 with
            | [] -> []
            | (p, el)::t -> 
                if not_contains p l1 then (p, el)::(get_different_phis l1 t)
                else get_different_phis l1 t

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
            mergedList@(get_different_phis mergedList l2)

        (* parcours de l'arbre pour constituer la liste de mots *)
        let rec parcours_comp_phi abr = 
                match abr with
                | ABRP_Empty -> []
                | ABRP_Node(ABRP_Empty, k, ABRP_Empty, p) -> [(p, [k])]
                | ABRP_Node(ABRP_Empty, k, right, p) -> (p, [k])::(parcours_comp_phi right)
                | ABRP_Node(left, k, ABRP_Empty, p) -> (p, [k])::(parcours_comp_phi left)
                | ABRP_Node(left, k, right, p) -> (p, [k])::(merge_phi (parcours_comp_phi left) (parcours_comp_phi right));;

        (* renvoie le mot associé à la valeur v *)
        let rec get_phi_from_val v abrp =
            match abrp with
            | ABRP_Empty -> assert false (* v est forcément dans l'arbre *)
            | ABRP_Node(l, k, r, phi) -> 
                if v = k then phi
                else if v < k then get_phi_from_val v l
                else get_phi_from_val v r
    end

 module Acl =
    struct
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
    end

module Ach =
    struct
        open Utils
        open Abr_phi
        open Acl

        type  abr_compressed_hashmap = 
            | ACH_Empty
            | ACH_Transition of int * ach_node ref
            | ACH_Node of ach_node
                and ach_node = {mutable fg: abr_compressed_hashmap; hmap: ((string, int) Hashtbl.t); mutable fd: abr_compressed_hashmap}

        (* convertit les branches d'un noeud d'ACL vers un noeud d'ACH *)
        let acl_node2ach_node aclNode hmap =
            let rec an2am_aux branchList =
                match branchList with
                | [] -> ()
                | branch::nextBranches ->
                    let key = int_list2string (List.tl branch.values) in
                    let value = List.hd branch.values in

                    Hashtbl.add hmap key value;
                    an2am_aux nextBranches
            in

            an2am_aux aclNode.branches
                
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

                    acl_node2ach_node aclNode achNode.hmap;      
                    Hashtbl.add pmap phiKey (ref achNode);

                    ACH_Node(achNode)
            in

            a2a_aux aclNode

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
                | _, _ -> ()
            in

            match acl, ach with
            | ACL_Node(aclNode), ACH_Node(achNode) ->
                ba_aux aclNode.fg aclNode achNode.fg achNode true;
                ba_aux aclNode.fd aclNode achNode.fd achNode false;
            | _, _ -> ()

        (* construit l'arbre compressé qui utilise une hashmap pour les transitions *)
        let create_ach acl abrp tailleListe =
            let phiHash = Hashtbl.create tailleListe in
            let ach = acl2ach acl abrp phiHash in

            build_ach acl ach abrp phiHash;
            ach

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

            af_aux ach ""

        (* renvoie le nombre de noeuds et de clés*)
        let ach_stats ach =
            let nbKeys = ref 0 in
            let nbNodes = ref 0 in
            let deepness = ref 0 in

            let rec as_aux ach curDeepness =
                match ach with
                | ACH_Empty -> ()
                | ACH_Transition(_) -> ()
                | ACH_Node(node) ->
                    if curDeepness > !deepness then deepness := curDeepness;

                    nbKeys := (!nbKeys + (Hashtbl.length node.hmap));
                    nbNodes := (!nbNodes + 1);
                    as_aux node.fg (curDeepness + 1);
                    as_aux node.fd (curDeepness + 1);
            in

            as_aux ach 1;
            (!nbNodes, !nbKeys) 
    end

module Acm = 
    struct
        open Utils
        open Abr_phi
        open Acl

        module StrMap = Map.Make(String)

        type  abr_compressed_map = 
            | ACM_Empty
            | ACM_Transition of int * acm_node ref
            | ACM_Node of acm_node
                and acm_node = {mutable fg: abr_compressed_map; map: int StrMap.t; mutable fd: abr_compressed_map}


        (* convertit les branches d'un noeud d'ACL vers un noeud d'ACM *)
        let rec acl_node2acm_node branchList map =
            match branchList with
            | [] -> map
            | branch::nextBranches ->
                let key = int_list2string (List.tl branch.values) in
                let value = List.hd branch.values in

                acl_node2acm_node nextBranches (StrMap.add key value map)

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

                    Hashtbl.add pmap phiKey (ref acmNode);
                    ACM_Node(acmNode)
            in

            a2a_aux aclNode

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
                | _, _ -> ()
            in

            match acl, acm with
            | ACL_Node(aclNode), ACM_Node(acmNode) ->
                ba_aux aclNode.fg aclNode acmNode.fg acmNode true;
                ba_aux aclNode.fd aclNode acmNode.fd acmNode false;
            | _, _ -> ()


        (* construit l'arbre compressé qui utilise une map pour les transitions *)
        let create_acm acl abrp tailleListe =
            let phiHash = Hashtbl.create tailleListe in
            let acm = acl2acm acl abrp phiHash in

            build_acm acl acm abrp phiHash;
            acm

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

            af_aux acm ""

        (* renvoie le nombre de noeuds et de clés*)
        let acm_stats acm =
            let nbKeys = ref 0 in
            let nbNodes = ref 0 in
            let deepness = ref 0 in

            let rec as_aux acm curDeepness =
                match acm with
                | ACM_Empty -> ()
                | ACM_Transition(_) -> ()
                | ACM_Node(node) ->
                    if curDeepness > !deepness then deepness := curDeepness;

                    nbKeys := (!nbKeys + (StrMap.cardinal node.map));
                    nbNodes := (!nbNodes + 1);
                    as_aux node.fg (curDeepness + 1);
                    as_aux node.fd (curDeepness + 1);
            in

            as_aux acm 1;
            (!nbNodes, !nbKeys)
    end

module Acme =
    struct
        open Utils
        open Abr_phi
        open Acl

        module StrMap = Map.Make(Int)

        type  abr_compressed_map_enhanced = 
            | ACME_Empty
            | ACME_Transition of int * acme_node ref
            | ACME_Node of acme_node
                and acme_node = {mutable fg: abr_compressed_map_enhanced; map: int StrMap.t; mutable fd: abr_compressed_map_enhanced}


        (* convertit les branches d'un noeud d'ACL vers un noeud d'ACME *)
        let rec acl_node2acme_node branchList map =
            match branchList with
            | [] -> map
            | branch::nextBranches ->
                let key = int_list2string (List.tl branch.values) in
                let value = List.hd branch.values in

                acl_node2acme_node nextBranches (StrMap.add (key_hash key) value map)

        (* convertit un ACL vers un ACME incomplet (sans les transitions) *)
        let acl2acme aclNode abrp pmap =
            let rec a2a_aux aclNode =
                match aclNode with
                | ACL_Transition(_) | ACL_Empty -> ACME_Empty
                | ACL_Node(aclNode) ->
                    let acmeNode = {
                        fg = a2a_aux aclNode.fg; 
                        map = acl_node2acme_node aclNode.branches StrMap.empty;
                        fd = a2a_aux aclNode.fd} in

                    let value = List.hd (List.hd aclNode.branches).values in
                    let phiKey = get_phi_from_val value abrp in

                    Hashtbl.add pmap phiKey (ref acmeNode);
                    ACME_Node(acmeNode)
            in

            a2a_aux aclNode

        (* complète l'ACME en ajoutant les transitions à partir de l'ACL *)
        let rec build_acme acl acme abrp pmap =

            (* met à jour les transitions du parent (fonction auxiliaire pour factoriser le code) *)
            let rec ba_aux aclSon aclParent acmeSon acmeParent isLeft =
                match aclSon, acmeSon with
                | ACL_Node(_), ACME_Node(_) -> build_acme aclSon acmeSon abrp pmap;
                | ACL_Transition(t, r), ACME_Empty ->
                    let value = List.hd (List.hd (!r).branches).values in
                    let phi = get_phi_from_val value abrp in
                    let node = Hashtbl.find pmap phi in

                    if isLeft then acmeParent.fg <- ACME_Transition(t, node)
                    else acmeParent.fd <- ACME_Transition(t, node);
                | _, _ -> ()
            in

            match acl, acme with
            | ACL_Node(aclNode), ACME_Node(acmeNode) ->
                ba_aux aclNode.fg aclNode acmeNode.fg acmeNode true;
                ba_aux aclNode.fd aclNode acmeNode.fd acmeNode false;
            | _, _ -> ()


        (* construit l'arbre compressé qui utilise une map pour les transitions *)
        let create_acme acl abrp tailleListe =
            let phiHash = Hashtbl.create tailleListe in
            let acme = acl2acme acl abrp phiHash in

            build_acme acl acme abrp phiHash;
            acme

        (* recherche dans un AcM *)
        let rec acme_find acme v =
            let rec af_aux acme transitions =
                match acme with
                | ACME_Empty -> false
                | ACME_Transition(t, r) -> 
                    af_aux (ACME_Node(!r)) ((string_of_int t) ^ ";" ^ transitions)

                | ACME_Node(node) ->
                    let valBranch = StrMap.find (key_hash transitions) node.map in

                    if v = valBranch then true
                    else if v < valBranch then af_aux node.fg transitions
                    else af_aux node.fd transitions
            in

            af_aux acme ""

        (* renvoie le nombre de noeuds et de clés*)
        let acme_stats acme =
            let nbKeys = ref 0 in
            let nbNodes = ref 0 in
            let deepness = ref 0 in

            let rec as_aux acme curDeepness =
                match acme with
                | ACME_Empty -> ()
                | ACME_Transition(_) -> ()
                | ACME_Node(node) ->
                    if curDeepness > !deepness then deepness := curDeepness;

                    nbKeys := (!nbKeys + (StrMap.cardinal node.map));
                    nbNodes := (!nbNodes + 1);
                    as_aux node.fg (curDeepness + 1);
                    as_aux node.fd (curDeepness + 1);
            in

            as_aux acme 1;
            (!nbNodes, !nbKeys) 
    end

module TreeGen = 
    struct
        open Utils
        open Abr
        open Abr_phi
        open Acl
        open Ach
        open Acm
        open Acme

        let gen_abr valList =
    list2abr valList

    (* génère un ABR aléatoire *)
    let gen_abr_from_rand listLen =
        let vals = gen_permutation listLen in
        gen_abr vals

    (* génère un ABR aléatoire *)
    let gen_abr_from_file filePath =
        let vals = get_test_list filePath in
        gen_abr (fst vals)


    let gen_acl valList listLen =
        let abr = list2abr valList in
        let abrPhi = abr2abrPhi abr in
        let phiList = parcours_comp_phi abrPhi in

        (create_acl abrPhi  phiList listLen), abrPhi

    (* génère un ACL aléatoire *)
    let gen_acl_from_rand listLen =
        let vals = gen_permutation listLen in
        fst (gen_acl vals listLen)

    (* génère un ACL à partir d'un jeu de tests *)
    let gen_acl_from_file filePath =
        let vals = get_test_list filePath in
        fst (gen_acl (fst vals) (snd vals))

    let gen_ach valList listLen =
        let genAcl = gen_acl valList listLen in
        create_ach (fst genAcl) (snd genAcl) listLen

    (* génère un ACH aléatoire *)
    let gen_ach_from_rand listLen =
        let vals = gen_permutation listLen in
        gen_ach vals listLen

    (* génère un ACH à partir d'un jeu de tests *)
    let gen_ach_from_file filePath =
        let vals = get_test_list filePath in
        gen_ach (fst vals) (snd vals) 


    let gen_acm valList listLen =
        let genAcl = gen_acl valList listLen in
        create_acm (fst genAcl) (snd genAcl) listLen

    (* génère un ACH aléatoire *)
    let gen_acm_from_rand listLen =
        let vals = gen_permutation listLen in
        gen_acm vals listLen

    (* génère un ACH à partir d'un jeu de tests *)
    let gen_acm_from_file filePath =
        let vals = get_test_list filePath in
        gen_acm (fst vals) (snd vals);;

        let gen_acme valList listLen =
            let genAcl = gen_acl valList listLen in
            create_acme (fst genAcl) (snd genAcl) listLen

        (* génère un ACH aléatoire *)
        let gen_acme_from_rand listLen =
            let vals = gen_permutation listLen in
            gen_acme vals listLen

        (* génère un ACH à partir d'un jeu de tests *)
        let gen_acme_from_file filePath =
            let vals = get_test_list filePath in
            gen_acme (fst vals) (snd vals);;
    end