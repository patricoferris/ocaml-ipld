module type S = sig
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
  (** The IPLD data model. *)

  val find : t -> string -> (t option, [ `Msg of string ]) result
  (** [find t k] looks up the key [k] in the map [t]. If [t] is not a map
    the function returns an error. If the key is not found the function
    returns [Ok None]. *)

  val find_exn : t -> string -> t
  (** Exactly like {! find} only using exceptions. 
    @raise Invalid_argument if a non-map value is given.
    @raise Not_found if the key is not in the map. *)

  val pp : Format.formatter -> t -> unit
  (** A pretty-printer for values of type {! t}. *)

  (** {2 Combinators} 
    
    These functions allow you to contruct new IPLD values from OCaml values. *)

  val null : t
  (** A value for [`Null] *)

  val bool : bool -> t
  (** [bool b] is [`Bool b]. *)

  val int : int -> t
  (** [int i] is [`Int i]. *)

  val float : float -> t
  (** [float f] is [`Float f]. *)

  val string : string -> t
  (** [string s] is [`String s]. *)

  val bytes : bytes -> t
  (** [bytes b] is [`Bytes b]. *)

  val link : Cid.t -> t
  (** [link c] is [`Link c]. *)

  val list : ('a -> t) -> 'a list -> t
  (** [list fn lst] is [`List (List.map fn lst)]. *)

  val map : (string * t) list -> t
  (** [map assoc] is [`Map assoc]. *)
end

module type Intf = sig
  include S

  module type Codec = sig
    type outer := t
    type t

    val encode : t -> outer
    (** [encode t] encodes a value of type [t] to the IPLD value. *)

    val decode : outer -> t
    (** [decode ipld] takes the IPLD value [ipld] and decodes in the Codec's value. *)
  end
end
