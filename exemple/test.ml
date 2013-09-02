type location = {
  lat : float;
  lon : float;
} deriving (Yojson)

type t = {
  name : string;
  loc : location option
} deriving (Yojson)


let value_from_string = Yojson_t.from_string
    ~o:{name="name";loc=None} "{\"loc\":{\"lat\": 5.2, \"lon\": 9.4}}"

let other = {name="name"; loc=Some {lat = 5.2; lon = 9.4} }

let s = Yojson_t.to_string ~filter:[["loc";"some";"lat"];["name"]] other

let _ =
  print_endline s;
  assert (other = value_from_string);
  print_endline "passed"
