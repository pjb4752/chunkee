module Segment: sig
  type t

  val from_string: string -> t

  val compare: t -> t -> int

  val to_string: t -> string

  val inspect: t -> string
end

module Path: sig
  type t

  val from_segments: Segment.t list -> t

  val to_segments: t -> Segment.t list
end

type t

val from_path_and_base: Path.t -> Segment.t -> t

val base: t -> Segment.t

val path: t -> Path.t

val to_segments: t -> Segment.t list

val path_segments: t -> Segment.t list

val to_string: t -> string

val inspect: t -> string
