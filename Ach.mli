type  abr_compressed_hashmap = 
    | ACH_Empty
    | ACH_Transition of int * ach_node ref
    | ACH_Node of ach_node
        and ach_node = {mutable fg: abr_compressed_hashmap; hmap: ((string, int) Hashtbl.t); mutable fd: abr_compressed_hashmap}

val acl_node2ach_node: 
    Acl.acl_node -> 
    (string, int) Hashtbl.t -> unit

val acl2ach:
    Acl.abr_compressed_list -> 
    Abr_phi.abr_phi -> 
    (string, ach_node ref) Hashtbl.t -> abr_compressed_hashmap

val build_ach: 
    Acl.abr_compressed_list -> 
    abr_compressed_hashmap -> 
    Abr_phi.abr_phi -> 
    (string, ach_node ref) Hashtbl.t -> unit

val create_ach: 
    Acl.abr_compressed_list -> 
    Abr_phi.abr_phi -> 
    int -> abr_compressed_hashmap

val ach_find: 
    abr_compressed_hashmap -> 
    int -> bool