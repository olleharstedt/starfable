#!/usr/bin/env ocaml

Random.self_init ();;

type race = 
  | Rabbit
  | Lion
  | Rat
  | Bird

type age_category =
  | Young
  | Middle_age
  | Old
;;

type basic_properties = {
  contacts    : int;
  resources   : int;
  smart       : int;
  learning    : int;
  agressivity : int;
  diplomacy   : int;
};;

type character = {
  race : race;
  age : int;
  age_category : age_category;
  basic_properties : basic_properties;
};;

let category_of_age age =
  if age >= 0 && age <= 25 then Young
  else if age > 25 && age <= 45 then Middle_age
  else Old
;;

let dice () = (Random.int 6) + 1;;

let two_dice () = dice () + dice ();;

let race =
  match Random.int 4 with
  | 0 -> "kanin"
  | 1 -> "lejon"
  | 2 -> "råtta"
  | 3 -> "fågel"
  | _ -> assert false
;;

let age = 
  match race with
  | "kanin" -> (two_dice ()) * 6
  | "fågel" -> (two_dice ()) * 8
  | "lejon" -> (two_dice ()) * 7
  | "råtta" -> (two_dice ()) * 6

let gender = match Random.int 2 with
  | 0 -> "Manlig"
  | 1 -> "Kvinnlig"
  | _ -> assert false

let basic_property name =
  print_endline (name ^ ":\t" ^ (string_of_int (two_dice ())))
;;

(**
 * @return basic_properties
 *)
let apply_age_category cat props =
  match cat with
  | Young -> {
      props with 
      agressivity = props.agressivity + 1;
      learning    = props.learning - 1;
    }
  | Old -> {
      props with
      agressivity = props.agressivity - 1;
      learning    = props.learning + 1
    }
  | _ -> props
;;

let age_cat = category_of_age age;;

(**
 * @return basic_properties
 *)
let basic_properties = apply_age_category age_cat {
  contacts    = two_dice ();
  resources   = two_dice ();
  smart       = two_dice ();
  learning    = two_dice ();
  agressivity = two_dice ();
  diplomacy   = two_dice ();
};;

(**
 * @return string
 *)
let education props = 
  match props.learning with
  | 2 -> "Analfabet"
  | 3 -> "Kan läsa och räkna, men inget mer"
  | 4 | 5 -> "Sex års grundskolgång"
  | 6 | 7 -> "Nio års grundskola"
  | 8 | 9 -> "Grundskola samt två år på universitet"
  | 10 -> "Grundskola samt fem år på universitet"
  | 11 -> "Grundskola samt akademisk karriär"
  | 12 -> "Professor, otroligt lärd"
  | _ -> assert false
;;

let jobs props =
  match props.resources with
  | 2 -> "Har aldrig haft ett jobb, tiggare"
  | 3 -> "Tillfälliga ströjobb, mest arbetslös"
  | 4 | 5 -> "Enkelt jobb, låg lön"
  | 6 | 7 -> "Stabilt jobb"
  | 8 | 9 -> "Jobb med prestige och bra villkor, semester, sjukvård"
  | 10 | 11 -> "Chef"
  | 12 -> "Ägare och kung"
  | _ -> assert false
;;

let social props =
  match props.contacts with
  | 2 -> "Totalt isolerad"
  | 3 -> "Ensam, kanske en vän eller bekant"
  | 4 | 5 -> "Har familj men inga vänner"
  | 6 | 7 -> "Familj och några vänner"
  | 8 | 9 -> "Är social, utåtriktad, har många vänner och kontakter"
  | 10 | 11 ->  "Stor familj och många vänner och kontakter i höga positioner"
  | 12 -> "Känner alla och kan träffa vem som helst vid behov"
;;


(**
 * @return void
 *)
let print_basic_properties props =
  Printf.printf "Contacts: \t%d\n" props.contacts;
  Printf.printf "Resources: \t%d\n" props.resources;
  Printf.printf "Smart:   \t%d\n" props.smart;
  Printf.printf "Learning: \t%d\n" props.learning;
  Printf.printf "Agressivity: \t%d\n" props.agressivity;
  Printf.printf "Diplomacy: \t%d\n" props.diplomacy;;

Printf.printf "%s %s, %d år gammal\n"
  gender
  race
  age
;;
print_basic_properties basic_properties;;
print_endline (education basic_properties);;
print_endline (jobs basic_properties);;
print_endline (social basic_properties);;
