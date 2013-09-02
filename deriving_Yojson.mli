type json = Yojson.Safe.json

module type Yojson'' = sig
  type a
  val to_json : ?filter:string list list -> a -> json
  val from_json : ?o:a -> json -> a
end

module type Yojson = sig
  type a
  val to_json : ?filter:string list list -> a -> json
  val to_string : ?filter:string list list -> a -> string
  val from_json : ?o:a -> json -> a
  val from_string : ?o:a -> string -> a
end

module Defaults(D : Yojson'') : Yojson with type a = D.a

exception Expected_type of string * json
exception Failed

module Yojson_string : Yojson with type a = string
module Yojson_int64 : Yojson with type a = int64
module Yojson_int32 : Yojson with type a = int32
module Yojson_int : Yojson with type a = int
module Yojson_bool : Yojson with type a = bool
module Yojson_unit : Yojson with type a = unit
module Yojson_char : Yojson with type a = char
module Yojson_float : Yojson with type a = float
module Yojson_list(Y : Yojson''): Yojson with type a = Y.a list
module Yojson_option(Y : Yojson''): Yojson with type a = Y.a option
module Yojson_array(Y : Yojson''): Yojson with type a = Y.a array
module Yojson_json: Yojson with type a = json

val warning : ('a, unit, string, unit) format4 -> 'a
val set_warning : (string -> unit) -> unit

(* internals *)
type filter = string list list option
val expected_error : string -> json -> 'a
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val option_map : ('a -> 'b) -> 'a option -> 'b option
val leaf : filter -> json -> json
val find : string -> filter -> (bool -> filter -> 'a) -> 'a
