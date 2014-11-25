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

exception No_node_content of string
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
 * Building blocks of a rendered ship
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
 * Fetch node which tag name = tag_name
 *
 * @param xml Xml.t
 * @param node string
 * @return Xml.Element
 *)
let rec fetch_node xml tag_name = 

  (**
   * Search xml_list for tag
   *
   * @param xml_list Xml.xml list
   * @param tag_name string
   * @return Xml.Element
   *)
  let rec search_xml_list xml_list tag_name = match xml_list with
      [] -> raise Not_found
    | x::xs -> match x with
        Xml.Element (tag, _, _) -> 
          if tag = tag_name then x else search_xml_list xs tag_name
      | Xml.PCData _ -> search_xml_list xs tag_name
  in
  match xml with 
    Xml.Element (tag, attrs, []) -> assert false
  | Xml.Element (tag, attrs, children) -> 
      (* TODO: iter depth and bredth *)
      if tag = tag_name then xml else search_xml_list children tag_name
  | Xml.PCData text -> assert false

(**
 * Fetch the text content of a node, like
 * <example>bla</example>
 *
 * @param xml Xml.Element
 * @return string
 * @raise No_node_content if @xml is not last children in list
 *)
let fetch_content xml = match xml with
    Xml.Element (_, _, Xml.PCData text :: []) -> text
  | Xml.Element (tag, _, _) -> raise (No_node_content tag)
  | _ -> raise (No_node_content "(no tag)")

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
  type age_t = New | Old | Ancient
  type shiptype = Military of age_t | Civil of age_t
  type capacity = Tiny | Small | Medium | Large
  type builder = Lonely_genius | Syndicate | Family

  let age = match dice 3 with
      1 -> New
    | 2 -> Old
    | 3 -> Ancient
    | _ -> assert false

  let shiptype_of_int i = match i with
      1 -> Military age
    | 2 -> Civil age
    | _ -> assert false

  let string_of_shiptype shiptype = match shiptype with
      Military _ -> "military"
    | Civil _ -> "civil"

  let string_of_age age = match age with
      New -> "new"
    | Old -> "old"
    | Ancient -> "ancient"

  let string_of_builder b = match b with
      Lonely_genius -> "genius"
    | Syndicate -> "syndicate"
    | Family -> "family"

  let shiptype = shiptype_of_int (dice 2)

  let builder = match dice 3 with
      1 -> Lonely_genius
    | 2 -> Syndicate
    | 3 -> Family
    | _ -> assert false

  (*
  let age = match shiptype with
      Military -> (match dice 4 with
          1 -> dice 10
        | 2 -> dice 20
        | 3 -> dice 30
        | 4 -> (dice 40) + 20
        | _ -> assert false
      )
    | Civil -> (match dice 4 with
        1 -> dice 50
      | 2 -> dice 100
      | 3 -> dice 200
      | 4 -> dice 500
      | _ -> assert false
    )
  *)

  (** Sentences that build up the text. TODO: Could they be dependent on each other? *)
  (* TODO: How to add colour to the sentence? "If old and built by family, then add at the end..." *)
  (* TODO: Put numbers to template place holders *)
  let sentences = [
    {
      name = "shiptype"; 
      template = fetch_node_content Init.xml "shiptype_s"; 
      variables = [
        fetch_node_content Init.xml ("age_t_" ^ (string_of_age age));
        fetch_node_content Init.xml ("shiptype_" ^ (string_of_shiptype shiptype));
        fetch_node_content Init.xml ("builder_" ^ (string_of_builder builder));
      ]
    };
    (*
    {
      name = "age";
      template = fetch_node_content Init.xml "age_s";
      variables = [string_of_age age]
    }
    *)
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

  let text = List.fold_left (fun s x -> s ^ render_sentence x) "" A.sentences

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

  let xml = Xml.parse_file "spacestation.xml" in
  let lang_xml = fetch_node xml "lang" in
  print_endline (Xml.to_string lang_xml);
  let xml = fetch_node lang_xml (string_of_lang lang) in

  let config = {race = Rabbit; lang = lang; xml = xml} in

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
