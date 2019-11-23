open Gen
open Abr
open Abr_phi
open Acl
open Ach
open Acm

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