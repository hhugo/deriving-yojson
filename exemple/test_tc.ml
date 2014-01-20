open Sexplib
open Std

type location = {
  lat : float;
  lon : float;
} with sexp,yojson

type t = {
  name : string;
  loc : location option
} with sexp,yojson

let value_from_string = Yojson_t.from_string
    ~o:{name="name";loc=None} "{\"loc\":{\"lat\": 5.2, \"lon\": 9.4}}"

let other = {name="name"; loc=Some {lat = 5.2; lon = 9.4} }

let s = Yojson_t.to_string ~filter:[["loc";"some";"lat"];["name"]] other

let s_sexp = sexp_of_t other
let _ =
  print_endline s;
  print_endline (Sexp.to_string s_sexp);
  assert (other = value_from_string);
  print_endline "passed"
