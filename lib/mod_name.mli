module Segment: sig
  type t

  val from_string: string -> t

  val compare: t -> t -> int

  val to_string: t -> string

  val inspect: t -> string
end

module Path: sig
  type t

  val from_list: Segment.t list -> t

  val to_list: t -> Segment.t list
end

type t

val make: Path.t -> Segment.t -> t

val short_name: t -> Segment.t

val path_list: t -> Segment.t list

val to_string: t -> string

val to_list: t -> Segment.t list

val inspect: t -> string
