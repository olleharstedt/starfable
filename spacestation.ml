(*
 *
 * Render info space stations and space ships.
 * 
 * XML for translations.
 * Functor to simulate a "interface" ANIMALTYPE for those parts that
 * are specific for each animal.
 * The target is to create sentences? Or data? Visualized?
 *
 * @since 2014-10-29
 * @author Olle Härstedt <olleharstedt at yahoo dot com>
 *)

open Printf

exception No_node_content
exception Not_implemented
exception No_such_lang of string

(**
 * The four races
 *)
type race = Rabbit | Bird | Lion | Rat

(**
 * Languages
 *)
type lang = En | Sv

(**
 * Config
 *)
type config = {
  lang : lang;
  race : race;
  xml : Xml.xml
}

(**
 * Sentence type
 *)
type sentence = {
  name : string;
  template : string; 
  variables : string list
}

(**
 * String of lang
 *
 * @param l lang
 * @return string
 *)
let string_of_lang l = match l with
    En -> "en"
  | Sv -> "sv"

(**
 * Lang of string
 *
 * @param s string
 * @return lang
 *)
let lang_of_string s = match s with
    "en" -> En
  | "sv" -> Sv
  | _ -> raise (No_such_lang s)

(**
 * Insert variables into template place-holders
 * A place holder is a %
 *
 * @param sen sentence
 * @return string
 *)
let render_sentence sen = 
  let r = Str.regexp "\\%" in
  let rec replace s l = match l with
      [] -> s
    | x::xs -> replace (Str.replace_first r x s) xs
  in
  replace sen.template sen.variables
;;

(**
 * Race of int
 *
 * @param n int
 * @return race
 *)
let race_of_int n = match n with
    1 -> Rabbit
  | 2 -> Bird
  | 3 -> Lion
  | 4 -> Rat
  | _ -> assert false

(** Return number from 1 to n *)
let dice n = 
  Random.int n + 1

(**
 * String of race
 *
 * @param r race
 * @return string
 *)
let string_of_race r = match r with
    Rabbit -> "rabbit"
  | Bird -> "bird"
  | Lion -> "lion"
  | Rat -> "rat"

(**
 * Fetch children of node
 *
 * @param xml Xml.t
 * @param node string
 *)
let rec fetch_node xml tag_name = match xml with 
    Xml.Element (tag, attrs, children) -> 
      (* TODO: iter depth and bredth *)
      if tag == tag_name then children else fetch_node xmlss tag_name
  | Xml.Element (tag, attrs, []) -> assert false
  | Xml.PCData text -> assert false

let fetch_content xml = match xml with
    Xml.Element (tag_name, attrs, Xml.PCData text :: []) -> text
  | _ -> raise No_node_content

(**
 * Fetch the content of an Xml node
 *
 * @param xml Xml.xml
 * @param node string
 * @return string
 *)
let fetch_node_content xml node =
  let node = fetch_node xml node in
  fetch_content node

module type ANIMALTYPE = sig
  val age : unit -> int
  val sentences : sentence list
end

module type SHIPGENERATOR = sig
  val text : string
end

module type CONFIG = sig
  val lang : lang
end

(* Maybe use *)
module type INIT = sig
end

(* Maybe use *)
module type INIT_RABBIT = sig
  val shiptype : int
end

(**
 * Rabbit module
 *)
module RabbitModule (Init : sig val xml : Xml.xml end) : ANIMALTYPE = struct
  type shiptype = Military | Civil

  let shiptype_of_int i = match i with
      1 -> Military
    | 2 -> Civil
    | _ -> assert false

  let string_of_shiptype shiptype = match shiptype with
      Military -> "military"
    | Civil -> "civil"

  let shiptype = shiptype_of_int (dice 2)

  let sentences = [
    {
      name = "shiptype"; 
      template = fetch_node_content Init.xml "shiptype_s"; 
      variables = [fetch_node_content Init.xml ("shiptype_" ^ (string_of_shiptype shiptype))]
    }
  ]

  let age () = dice 10
end

(**
 * Functor to generate a ship generator
 * Code and sentences common for all ships
 *
 * @param A : ANIMALTYPE Animal module
 * @param B : CONFIG
 * @return SHIPGENERATOR
 *)
module Make_ShipGenerator (A : ANIMALTYPE) (C : CONFIG) : SHIPGENERATOR = struct

  let text = List.fold_left (fun s x -> s ^ x.template) "" A.sentences

end

(**
 * Get a ship generator for race
 *
 * @param race
 * @return SHIPGENERATOR
 *)
let get_shipgenerator config = 

  (* Config module same for all modules *)
  let module Config = struct
    let lang = config.lang
  end in

  match config.race with
      Rabbit ->
        (* Init stuff here to be able to make many ship generators in one run *)
        let module Init = struct 
          let xml = config.xml
        end in
        let module M = (Make_ShipGenerator (RabbitModule (Init)) (Config)) in 
        (module M : SHIPGENERATOR)
    | _ -> 
        raise Not_implemented
;;

(* Main *)
let _ =

  let lang = if Array.length Sys.argv == 2 then Sys.argv.(1) else "en" in

  let lang = lang_of_string lang in

  Random.self_init ();
  let race = race_of_int (dice 4) in

  let xml = Xml.parse_file "spacestation.xml" in
  let lang_xml = fetch_node xml "lang" in
  print_endline (Xml.to_string lang_xml);
  let xml = fetch_node lang_xml (string_of_lang lang) in

  let config = {race = Rabbit; lang = lang; xml = xml} in

  print_endline (fetch_node_content xml "asd");

  let (module S) = get_shipgenerator config in
  print_endline S.text

  (*
  let rabbit = fetch_node_content xml "rabbit" in
  print_endline rabbit;

  print_endline (render_sentence {name = "bla"; template = "asd % asd"; variables = ["aaa"]})
  *)
  
  (*
  let dtd = Dtd.parse_file "myfile.dtd" in
  let x = Dtd.prove (Dtd.check dtd) "start" x in
  *)
  (*
  print_endline (Xml.to_string x)
  *)
