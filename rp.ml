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
  health      : int;
};;

type character = {
  race : race;
  age : int;
  age_category : age_category;
  basic_properties : basic_properties;
  jobs : string list;
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
  | _ -> assert false

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
  health      = two_dice ();
};;

(**
 * @return string
 *)
let education props = 
  match props.learning with
  | 1 | 2 -> "Analfabet"
  | 3 -> "Kan läsa och räkna, men inget mer"
  | 4 | 5 -> "Sex års grundskolgång"
  | 6 | 7 -> "Nio års grundskola"
  | 8 | 9 -> "Grundskola samt två år på universitet"
  | 10 -> "Grundskola samt fem år på universitet"
  | 11 -> "Grundskola samt akademisk karriär"
  | 12 | 13 -> "Professor, otroligt lärd"
  | _ -> assert false
;;

let jobs props =
  match props.resources with
  | 2 -> "Har aldrig haft ett jobb, tiggare"
  | 3 -> "Arbetsloes"
  | 4 -> "Tillfälliga ströjobb, mest arbetslös"
  | 5 -> "Enkelt jobb, låg lön, kanske städare, sekreterare."
  | 6 | 7 -> "Stabilt jobb, industri, lärare.."
  | 8 | 9 -> "Jobb med prestige och bra villkor, semester, sjukvård"
  | 10 | 11 -> "Chef eller högt uppsatt"
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
  | _ -> assert false
;;

let weapon props =
  match props.agressivity with
  | 2 | 3 | 4 | 5 | 6 -> "Äger inget vapen"
  | 7 -> "Äger ett litet vapen"
  | 8 -> "Äger två små vapen"
  | 9 -> "Äger ett stort vapen"
  | 10 -> "Äger två stora vapen"
  | 11 -> "Äger ett stort antal olika vapen"
  | 12 -> "Äger vapen med militär styra, på gränsen till vad som är lagligt."
  | _ -> assert false
;;

let job =
  match basic_properties.resources with
  | -1 | 0 | 1 | 2 | 3 -> "Arbetsloes eller kriminell"
  | _ -> 
      begin match race with
      | "kanin" -> 
          begin match Random.int 8 with
          | 0 -> "Facklig byråkrat"
          | 1 -> "Praktisk industri, mekaniker"
          | 2 -> "Fraktpilot"
          | 3 -> "Kock"
          | 4 -> "Polis eller vakt"
          | 5 -> "Milis"
          | 6 -> "Sjuksyrra"
          | 7 -> "Hotellägare, pub eller dylikt"
          | _ -> assert false
          end
      | "råtta" ->
          begin match Random.int 4 with
          | 0 -> "Styrelseproffs"
          | 1 -> "Företagsägare"
          | 2 -> "Hacker"
          | 3 -> "Diversehandel, försäljare"
          | _ -> assert false
          end
      | "lejon" ->
          begin match Random.int 5 with
          | 0 -> "Statlig byråkrat"
          | 1 -> "Militär"
          | 2 -> "Specialtrupp"
          | 3 -> "Aristokrat"
          | 4 -> "Ritualledare"
          | _ -> assert false
          end
      | "fågel" ->
          begin match Random.int 2 with
          | 0 -> "Präst"
          | 1 -> "Arkitekt"
          | _ -> assert false
          end
      | _ -> assert false
      end
;;

(**
 * @return void
 *)
let print_basic_properties props =
  Printf.printf "Kontakter: \t%d\n" props.contacts;
  Printf.printf "Resurser: \t%d\n" props.resources;
  Printf.printf "Aggressivitet: \t%d\n" props.agressivity;
  Printf.printf "Diplomati: \t%d\n" props.diplomacy;
  Printf.printf "Smart:  \t%d\n" props.smart;
  Printf.printf "Lärdom: \t%d\n" props.learning;
  Printf.printf "Hälsa:  \t%d\n" props.health;
;;

Printf.printf "%s %s, %d år gammal\n"
  gender
  race
  age
;;
print_basic_properties basic_properties;;
print_endline (education basic_properties);;
print_endline (jobs basic_properties);;
print_endline (social basic_properties);;
(*print_endline (weapon basic_properties);;*)
print_endline (job);;
