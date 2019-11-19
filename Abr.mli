type abr = ABR_Empty | ABR_Node of (abr * int * abr);;

val print_abr: abr -> unit
val add: int -> abr -> abr
val list2abr: int list -> abr
val abr_find: abr -> int -> bool
