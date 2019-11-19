module StrMap: 
    sig
        type key = String.t
        type 'a t = 'a Map.Make(String).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val max_binding : 'a t -> key * 'a
        val choose : 'a t -> key * 'a
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    end

type  abr_compressed_map = 
    | ACM_Empty
    | ACM_Transition of int * acm_node ref
    | ACM_Node of acm_node
        and acm_node = {mutable fg: abr_compressed_map; map: int StrMap.t; mutable fd: abr_compressed_map}

val acl_node2acm_node: 
    Acl.acl_branch list -> 
    int StrMap.t -> int StrMap.t

val acl2acm: 
    Acl.abr_compressed_list -> 
    Abr_phi.abr_phi -> 
    (string, acm_node ref) Hashtbl.t -> abr_compressed_map

val build_acm: 
    Acl.abr_compressed_list ->
    abr_compressed_map ->
    Abr_phi.abr_phi ->
    (string, acm_node ref) Hashtbl.t -> unit

val create_acm: 
    Acl.abr_compressed_list -> 
    Abr_phi.abr_phi -> 
    int -> abr_compressed_map

val create_acm: 
    Acl.abr_compressed_list -> 
    Abr_phi.abr_phi -> 
    int -> abr_compressed_map

val acm_find : 
    abr_compressed_map -> 
    int -> bool