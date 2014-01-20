open Pa_deriving_common
open Utils

module Description : Defs.ClassDescription = struct
  let classname = "Yojson"
  let default_module = Some "Defaults"
  let runtimename = "Deriving_Yojson"
  let alpha = None
  let allow_private = true
  let predefs = [
    ["int"      ], ["Deriving_Yojson";"int"];
    ["bool"     ], ["Deriving_Yojson";"bool"];
    ["unit"     ], ["Deriving_Yojson";"unit"];
    ["char"     ], ["Deriving_Yojson";"char"];
    ["int32"    ], ["Deriving_Yojson";"int32"];
    ["Int32";"t"], ["Deriving_Yojson";"int32"];
    ["int64"    ], ["Deriving_Yojson";"int64"];
    ["Int64";"t"], ["Deriving_Yojson";"int64"];
    ["nativeint"], ["Deriving_Yojson";"nativeint"];
    ["float"    ], ["Deriving_Yojson";"float"];
    ["string"   ], ["Deriving_Yojson";"string"];
    ["list"     ], ["Deriving_Yojson";"list"];
    ["ref"      ], ["Deriving_Yojson";"ref"];
    ["option"   ], ["Deriving_Yojson";"option"];
    ["array"    ], ["Deriving_Yojson";"array"];
    ["json"                ], ["Deriving_Yojson";"json"];
    ["Safe";"json"         ], ["Deriving_Yojson";"json"];
    ["Yojson";"Safe";"json"], ["Deriving_Yojson";"json"];
  ]
  let depends = []
end

module Builder(Generator : Defs.Generator) = struct

  open Generator.Loc
  open Camlp4.PreCast
  open Description

  module Helpers = Generator.AstHelpers

  let wrap ~to_json ~from_json = [ <:str_item< value to_json ?filter o = $to_json$;>>;
                                   <:str_item< value from_json ?o json = $from_json$;>> ]


  let generator = (object (self)

    inherit Generator.generator

    method proxy unit =
      None, [ <:ident< to_json >>;
              <:ident< from_json >>]

    method tuple ctxt args  =
      let ids, patt, expr = Helpers.tuple (List.length args) in
      let id_ty = List.zip ids args in
      let to_json =
        let l = List.mapn ~init:1 (fun (id,ty) i ->
          let fi = Printf.sprintf "f%d" i in
          <:expr<
            Deriving_Yojson.find $str:fi$ filter (fun b filter ->
              if b then Some ($self#call_expr ctxt ty "to_json"$ ?filter $lid:id$) else None)
          >>
        ) id_ty in
        <:expr<
          let $patt$ = o in
          let l = Deriving_Yojson.filter_map (fun x -> x) $Helpers.expr_list l$ in
          `List l >> in

      let from_json =
        let l = List.map (fun (id, ty) ->
          let get id = <:expr@here< Deriving_Yojson.option_map (fun $patt$ -> $lid:id$) o >> in
          <:expr< $self#call_expr ctxt ty "from_json"$ ?o:$get id$ $lid:id$ >>
        ) id_ty in
        <:expr<match json with
            [ `List $Helpers.patt_list (List.map (fun x-> <:patt<$lid:x$>>) ids)$ ->
              $Helpers.tuple_expr l$
            | err -> Deriving_Yojson.expected_error $str:Printf.sprintf "List%d" (List.length l)$ err]
            >>
      in wrap ~from_json ~to_json

    method case ctxt (name, args) =
      match args with
        | [] ->
          let from_json = <:match_case@here< `Assoc [($str:name$,`Null)] -> $uid:name$>> in
          let to_json = <:match_case@here< $uid:name$ ->
            Deriving_Yojson.find $str:name$ filter (fun b filter ->
              if b
              then `Assoc [($str:name$,`Null)]
              else `Null)
            >> in
          to_json, from_json
        | _ ->
          let ids, patt, expr = Helpers.tuple (List.length args) in
          let id_ty = List.zip ids args in
          let from_json =
            let l = List.map (fun (id, ty) ->
              let get id = <:expr@here< match o with
                  [ Some ( $uid:name$ $patt$) -> Some $lid:id$
                  | _ -> None ] >> in
              <:expr< $self#call_expr ctxt ty "from_json"$ ?o:$get id$ $lid:id$>>
            ) id_ty in
            <:match_case@here<
              `Assoc [($str:name$,`List ($Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$))] ->
            ($uid:name$ $Helpers.tuple_expr l$)
            >> in
          let to_json =
            let l = List.mapn ~init:1 (fun (id, ty) i ->
              let fi = Printf.sprintf "f%d" i in
              <:expr@here<
                Deriving_Yojson.find $str:fi$ filter (fun b filter ->
                  if b then Some ($self#call_expr ctxt ty "to_json"$ ?filter $lid:id$) else None)
              >>) id_ty in
            <:match_case@here< $uid:name$ $patt$ ->
            Deriving_Yojson.find $str:name$ filter (fun b filter ->
              if b then `Assoc [($str:name$,`List (Deriving_Yojson.filter_map (fun x -> x) $Helpers.expr_list l$))] else `Null)
            >> in
          to_json,from_json

    method sum ?eq ctxt tname params constraints summands =
      let to_jsons,from_jsons = List.split (List.map (self#case ctxt) summands) in
      let to_json = <:expr< match o with [$list:to_jsons$] >> in
      let fail = <:match_case< err -> Deriving_Yojson.expected_error "Assoc(_)" err>> in
      let from_json = <:expr< match json with [$list:from_jsons$ | $fail$] >> in
      wrap ~to_json ~from_json


    method record ?eq ctxt tname params constraints fields =
      let strip_underscore s = s in
      let all = List.map (fun (name,ty,_) -> name,strip_underscore name,ty) fields in
      let from_json =
        let l = List.map (fun (fname,strip_name,ty) ->
          let v = <:expr@here<
            try
              let j = List.assoc $str:strip_name$ l in
              $self#call_poly_expr ctxt ty "from_json"$ ?o:(Deriving_Yojson.option_map (fun v -> v.$lid:fname$) o) j
            with
                [ Not_found -> match o with
                    [ Some v -> v.$lid:fname$
                    | _ -> do{
                      Deriving_Yojson.warning $str:Printf.sprintf "no default for field %s" fname$;
                      $self#call_poly_expr ctxt ty "from_json"$ ?o:None `Null
                    }
                    ]
                  | Deriving_Yojson.Failed ->
                    do {
                      Deriving_Yojson.warning "wrong type for field %s" $str:fname$;
                      raise Deriving_Yojson.Failed
                    }
                  | _ ->
                    do {
                      Deriving_Yojson.warning "error for field %s" $str:fname$;
                      raise Deriving_Yojson.Failed
                    } ]
                >> in
          fname,v
        ) all in
        <:expr<match json with
            [ `Assoc l -> $Helpers.record_expr l$
            | err -> Deriving_Yojson.expected_error "Record" err] >> in
      let to_json =
        let l = List.map (fun (fname,strip_name,ty) ->
          <:expr<
            Deriving_Yojson.find $str:fname$ filter (fun b filter ->
              if b then Some (($str:fname$,$self#call_poly_expr ctxt ty "to_json"$ ?filter o.$lid:fname$)) else None)
          >>) all
        in <:expr< `Assoc (Deriving_Yojson.filter_map (fun x -> x) $Helpers.expr_list l$) >>
      in
      wrap ~to_json ~from_json

    method polycase ctxt  = function
      | Type.Tag (name, []) ->
        let from_json = <:match_case@here< `Assoc [($str:name$,`Null)] -> `$name$>> in
        let to_json = <:match_case@here< `$name$ ->
          Deriving_Yojson.find $str:name$ filter (fun b filter ->
            if b
            then `Assoc [($str:name$,`Null)]
            else `Null)
          >> in
        to_json, from_json
      | Type.Tag (name, args) ->
        let ids, patt, expr = Helpers.tuple (List.length args) in
        let id_ty = List.zip ids args in
        let from_json =
          let l = List.map (fun (id, ty) ->
            let get id = <:expr@here< match o with
                [ Some ( `$name$ $patt$) -> Some $lid:id$
                | _ -> None ] >> in
            <:expr< $self#call_expr ctxt ty "from_json"$ ?o:$get id$ $lid:id$>>
          ) id_ty in
          <:match_case@here<
            `Assoc [($str:name$,`List ($Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$))] ->
          (`$name$ $Helpers.tuple_expr l$)
          >> in
        let to_json =
          let l = List.mapn ~init:1 (fun (id, ty) i ->
            let fi = Printf.sprintf "f%d" i in
            <:expr@here<
              Deriving_Yojson.find $str:fi$ filter (fun b filter ->
                if b then Some ($self#call_expr ctxt ty "to_json"$ ?filter $lid:id$) else None)
            >>) id_ty in
          <:match_case@here< `$name$ $patt$ ->
          Deriving_Yojson.find $str:name$ filter (fun b filter ->
            if b then `Assoc [($str:name$,`List (Deriving_Yojson.filter_map (fun x -> x) $Helpers.expr_list l$))] else `Null)
          >> in
        to_json,from_json
      | Type.Extends t ->
        let patt, guard, cast = Generator.cast_pattern ctxt t in
        let to_json = <:match_case< $patt$ when $guard$ -> $self#call_expr ctxt t "to_json"$ ?filter $cast$ >> in
        let from_json = <:match_case< _ -> assert False >> (* TODO *) in
        to_json,from_json

    method variant ctxt tname params constraints (_,tags) =
      let to_jsons,from_jsons = List.split (List.map (self#polycase ctxt) tags) in
      let to_json = <:expr< match o with [$list:to_jsons$] >> in
      let fail = <:match_case< err -> Deriving_Yojson.expected_error "Assoc(_)" err>> in
      let from_json = <:expr< match json with [$list:from_jsons$ | $fail$] >> in
      wrap ~to_json ~from_json

  end :> Generator.generator)

  let classname = Description.classname
  let runtimename = Description.runtimename
  let generate = Generator.generate generator
  let generate_sigs = Generator.generate_sigs generator
  let generate_expr = Generator.generate_expr generator

end

include Base.RegisterFullClass(Description)(Builder)
