module State: sig
  type t

  val make: unit -> t
end

val emit_node: State.t -> Resolve.t -> string

val emit: Resolve.t list -> string list
