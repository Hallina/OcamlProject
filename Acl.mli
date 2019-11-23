type  abr_compressed_list = 
    | ACL_Empty
    | ACL_Transition of int * acl_node ref
    | ACL_Node of acl_node 
        and acl_node = {mutable fg: abr_compressed_list; branches: acl_branch list; mutable fd: abr_compressed_list}
        and acl_branch = {mutable values: int list}


val create_acl:
     Abr_phi.abr_phi -> 
     (string * int list) list -> 
     int -> abr_compressed_list

val acl_find: 
    abr_compressed_list -> 
    int -> bool

val acl_stats: 
    abr_compressed_list -> int * int