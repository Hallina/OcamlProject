type abr_phi = ABRP_Empty | ABRP_Node of (abr_phi * int * abr_phi * string)

val print_abrPhi: 
    abr_phi -> unit

val phi: 
    Abr.abr -> string

val abr2abrPhi: 
    Abr.abr -> abr_phi

val print_phiList: 
    (string * int list) list -> unit

val get_vals_of_phi: 
    ('a * 'b list) list -> 
    'a -> 'b list

val not_contains: 
    'a -> 
    ('a * 'b) list  -> bool

val get_different_phis: 
    (string * int) list -> 
    (string * int) list -> (string * int) list

val parcours_comp_phi: 
    abr_phi -> (string * int list) list

val get_phi_from_val: 
    int -> 
    abr_phi -> string