open Printf
open Tree
open Utils
open Abr
open Abr_phi
open Acl
open Ach
open TreeGen;;

open Printf
open Acl
open TreeGen;;

if Array.length Sys.argv < 2 then failwith "No input file.";
if Array.length Sys.argv < 3 then failwith "No dump file.";

let input = Sys.argv.(1) in
let dump = Sys.argv.(2) in

let ach = gen_ach_from_file input in

let achStats = ach_stats ach in
let nbNodes = fst achStats in
let keyMean = (float_of_int (snd achStats)) /. (float_of_int nbNodes) in 
let mem = Obj.reachable_words (Obj.repr ach) in

let abr = gen_abr_from_file input in
let deepestVal = List.nth (abr_stats abr) 2 in

let execTime = ref 0. in

for i = 1 to 100 do
    let t = Sys.time () in
    let _ = ach_find ach deepestVal in
    let time = (Sys.time ()) -. t in

    execTime := !execTime +. time;
done;

execTime := !execTime /. 100.;

let oc = open_out_gen [Open_append; Open_creat] 0o666 dump in

try 
    fprintf oc "%s,%d,%f,%d,%g,%d\n" input mem !execTime nbNodes keyMean deepestVal;
    flush oc; 
    close_out oc
with e -> 
    close_out_noerr oc;

    let msg = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s%s\n" msg stack;

exit 0