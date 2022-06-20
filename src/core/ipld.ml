type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Bytes of bytes
  | `Link of Cid.t
  | `List of t list
  | `Map of (string * t) list ]

let name = function
  | `Null -> "null"
  | `Bool _ -> "bool"
  | `Int _ -> "Int"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `Bytes _ -> "bytes"
  | `Link _ -> "link"
  | `List _ -> "list"
  | `Map _ -> "map"

let find_exn t s =
  match t with
  | `Map assoc -> List.assoc s assoc
  | _ -> invalid_arg ("Expected a map but got a " ^ name t)

let find t s =
  match t with
  | `Map assoc -> Ok (List.assoc_opt s assoc)
  | _ -> Error (`Msg ("Expected a map but got a " ^ name t))

let rec pp ppf = function
  | `Null -> Format.pp_print_string ppf "Null"
  | `Bool b -> Format.fprintf ppf "Bool(%b)" b
  | `Int i -> Format.fprintf ppf "Int(%i)" i
  | `Float f -> Format.fprintf ppf "Float(%f)" f
  | `String s -> Format.fprintf ppf "String(%s)" s
  | `Bytes b -> Format.fprintf ppf "Bytes(%a)" Format.pp_print_bytes b
  | `Link cid -> Format.fprintf ppf "Link(%a)" Cid.pp_human cid
  | `List lst -> Format.fprintf ppf "[ %a ]" (Format.pp_print_list pp) lst
  | `Map assoc -> Format.fprintf ppf "{ %a }" (Format.pp_print_list pp_kv) assoc

and pp_kv ppf (k, t) = Format.fprintf ppf "%s: %a" k pp t

let null = `Null
let bool b = `Bool b
let int i = `Int i
let float f = `Float f
let string s = `String s
let bytes b = `Bytes b
let link c = `Link c
let list fn lst = `List (List.map fn lst)
let map assoc = `Map assoc

module type Codec = sig
  type outer := t
  type t

  val encode : t -> outer
  val decode : outer -> t
end
