val gen_abr_from_rand: int -> Abr.abr
val gen_abr_from_file: string -> Abr.abr

val gen_acl_from_rand: int -> Acl.abr_compressed_list
val gen_acl_from_file: string -> Acl.abr_compressed_list

val gen_ach_from_rand: int -> Ach.abr_compressed_hashmap
val gen_ach_from_file: string -> Ach.abr_compressed_hashmap

val gen_acm_from_rand: int -> Acm.abr_compressed_map
val gen_acm_from_file: string -> Acm.abr_compressed_map


