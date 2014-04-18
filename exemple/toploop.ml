#use "topfind";;
#camlp4o;;
#require "deriving.syntax";;
(* #require "deriving.syntax.tc";; *)
#require "deriving-yojson.syntax";;
#require "deriving-yojson";;
type t = int deriving(Yojson)
(* type t = int with yojson;; *)
