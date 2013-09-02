Deriving-yojson
===============

Deriving-yojson is a Deriving (https://github.com/ocsigen/deriving) extention to print / parse ocaml value to / from json.
Json manipulation is done by Yojson (https://github.com/mjambon/yojson/)

Exemple
-------
```
type location = {
  lat : float;
  lon : float;
} deriving (Yojson)

type t = {
  name : string;
  loc : location option
} deriving (Yojson)
```

Parsing
-------

```
let value_from_json = Yojson_t.from_json ?o json
let value_from_string = Yojson_t.from_string ?o string
```
The parameter o is an optionnal default value. If the json is
incomplete, this default value will provide a value for missing fields
```
let value_from_string = Yojson_t.from_string
    ~o:{name="name";loc=None} "{\"loc\":{\"lat\": 5.2, \"lon\": 9.4}}"
```

Printing
--------
```
let s = Yojson_t.to_string ?filter t
```
the optionnal parameter filter allow to filter the output.
```
let s = Yojson_t.to_string ~filter:[["loc";"some";"lat"];["name"]] value_from_string
```
will ouput
```
{"name":"name","loc":{"lat":5.2}}
```
