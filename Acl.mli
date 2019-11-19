type  abr_compressed_list = 
    | ACL_Empty
    | ACL_Transition of int * acl_node ref
    | ACL_Node of acl_node 
        and acl_node = {mutable fg: abr_compressed_list; branches: acl_branch list; mutable fd: abr_compressed_list}
        and acl_branch = {mutable values: int list}

val print_branches: 
    acl_node -> unit

val print_acl: 
    abr_compressed_list -> unit

val elems2node_list: 
    int list -> acl_branch list

val add_acl:
    abr_compressed_list -> 
    'a -> int list -> 
    ('a, acl_node ref) Hashtbl.t -> unit

val phiList2acl: 
    ('a * int list) list -> 
    ('a, acl_node ref) Hashtbl.t -> abr_compressed_list

val get_prev_val_and_phi: 
    int -> 
    Abr_phi.abr_phi -> (int * string)

val get_branch_from_branchList:
     int -> 
     acl_branch list -> acl_branch

val build_acl_links: 
    acl_branch list -> 
    acl_node -> 
    Abr_phi.abr_phi -> 
    (string, acl_node ref) Hashtbl.t -> 
    <new_id: int; get_id: int> -> unit

val create_acl:
     Abr_phi.abr_phi -> 
     (string * int list) list -> 
     int -> abr_compressed_list

val get_branch_by_transitions: 
    acl_node -> 
    int list -> acl_branch

val acl_find: 
    abr_compressed_list -> 
    int -> bool