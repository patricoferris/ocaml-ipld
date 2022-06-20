type t = CBOR.Simple.t

let rec encode : t -> Ipld.t = function
  | `Null -> `Null
  | `Undefined -> `Null
  | `Simple _ -> assert false
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `Bytes b -> `Bytes (Bytes.of_string b)
  | `Text s -> `String s
  | `Array ts -> `List (List.map encode ts)
  | `Map assoc ->
      let string_map =
        List.map
          (function
            | `Text s, v -> (s, encode v)
            | _ -> invalid_arg "Maps should only used strings")
          assoc
      in
      `Map string_map
  | `Tag (42, `Bytes t) -> `Link (Cid.of_string t)
  | `Tag _ -> invalid_arg "Tag of 42 for links is the only supported tag"

let rec decode : Ipld.t -> t = function
  | (`Null | `Bool _ | `Int _ | `Float _) as v -> v
  | `String s -> `Text s
  | `Bytes b -> `Bytes (Bytes.to_string b)
  | `Link cid -> (
      match Cid.to_string cid with
      | Ok v -> `Tag (42, `Bytes v)
      | Error (`Msg m) -> failwith m)
  | `List lst -> `Array (List.map decode lst)
  | `Map assoc ->
      let assoc = List.map (fun (s, v) -> (`Text s, decode v)) assoc in
      `Map assoc
