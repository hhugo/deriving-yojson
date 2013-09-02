module type HJson = sig
  type a
  val to_json : ?filter:string list list -> a -> Yojson.Safe.json
  val from_json : ?o:a -> Yojson.Safe.json -> a
end

module Defaults(D : HJson) : HJson with type a = D.a = struct
  include D
end

exception Expected_type of string * Yojson.Safe.json
exception Failed

let expected_error name json = raise (Expected_type(name, json))

let rec filter_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x::xs -> match f x with
        | None -> loop acc xs
        | Some x -> loop (x::acc) xs
  in loop [] l

let warning = Printf.eprintf

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let leaf filter exp =
  match filter with
    | None
    | Some [[]] -> exp
    | Some l ->
      let () = warning "other cases %S" (String.concat ", " (List.map List.hd l)) in
      failwith "Issue with the filter"

let find str filter expr =
  match filter with
    | None -> expr true filter
    | Some [] -> expr false filter
    | Some l ->
      let filter = filter_map (function
        | x::xs when x = str-> Some xs
        | [] -> Some []
        | _ -> None ) l in
      match filter with
        | [] -> expr false (Some filter)
        | _ ->  expr true  (Some filter)


module HJson_string = Defaults(struct
  type a = string
  let from_json ?o = function
    | `String s -> s
    | `Int i -> string_of_int i
    | `Null -> ""
    | err -> expected_error "string" err
  let to_json ?filter s = leaf filter (`String s)
end)

module HJson_int64 = Defaults(struct
  type a = int64
  let from_json ?o = function
    | `Int i -> Int64.of_int i
    | `String s -> Int64.of_string s
    | err -> expected_error "int" err
  let to_json ?filter i = leaf filter (`Int (Int64.to_int i))
end)


module HJson_int = Defaults(struct
  type a = int
  let from_json ?o = function
    | `Int i -> i
    | `String s -> int_of_string s
    | err -> expected_error "int" err
  let to_json ?filter i = leaf filter (`Int i)
end)

module HJson_bool = Defaults(struct
  type a = bool
  let from_json ?o = function
    | `Bool b -> b
    | err -> expected_error "bool" err
  let to_json ?filter b = leaf filter (`Bool b)
end)

module HJson_unit = Defaults(struct
  type a = unit
  let from_json ?o _ = ()
  let to_json ?filter _ = leaf filter (`Null)
end)

module HJson_char = Defaults(struct
  type a = char
  let from_json ?o = function
    | `String s when String.length s = 1  -> s.[0]
    | `Int i -> char_of_int i
    | err -> expected_error "char" err
  let to_json ?filter c = leaf filter (`String (Printf.sprintf "%c" c))
end)

module HJson_float = Defaults(struct
  type a = float
  let from_json ?o = function
    | `Int i -> float_of_int i
    | `String s -> float_of_string s
    | `Float f -> f
    | err -> expected_error "float" err
  let to_json ?filter f = leaf filter (`Float f)
end)

module HJson_list (A : HJson) = Defaults(struct
  type a = A.a list
  let from_json ?o = function
    | `Null -> []
    | `List l ->
      begin
        match o with
          | Some v when (List.length v = List.length l) ->
            List.map2 (fun o json -> A.from_json ~o json) v l
          | _ -> List.map (fun json -> A.from_json ?o:None json ) l
      end
    | err -> expected_error "list" err
  let to_json ?filter l = `List (List.map (A.to_json ?filter) l)
end)

module HJson_option (A : HJson) = Defaults(struct
  type a = A.a option
  let from_json ?o json=
    match json,o with
      | (`Null,_) -> None
      | (s,(Some (Some o))) -> Some (A.from_json ~o s)
      | (s,_) -> Some (A.from_json ?o:None s)

  let to_json ?filter = function
    | None -> `Null
    | Some v ->
      find "some" filter
        (fun b filter ->
          if b
          then A.to_json ?filter v
          else `Null)
end)

module HJson_array (A : HJson) = Defaults(struct
  type a = A.a array
  let from_json ?o = function
    | `List l ->
      begin
        match o with
          | Some v when (Array.length v = List.length l) ->
            let v = Array.to_list v in
            let l = List.map2 (fun o json -> A.from_json ~o json) v l in
            Array.of_list l
          | _ ->
            let l = List.map (fun json -> A.from_json ?o:None json ) l in
            Array.of_list l
      end
    | `Null -> [||]
    | err -> expected_error "list" err
  let to_json ?filter l = `List (List.map (A.to_json ?filter) (Array.to_list l))
end)

module HJson_json = Defaults(struct
  type a = Yojson.Safe.json
  let from_json ?o x = x
  let to_json ?filter x = x
end)

module Json_json = struct
  type a = Yojson.Safe.json
  let write _ _ = let () = assert false in ()
  let read _ = let () = assert false in `Null
end
