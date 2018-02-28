module Listener : sig
  (* Resources associated to a ['a Listener.t] (subscription to events
     broadcasted by a ['a Signal.t]) are manually managed.

     Attaching a listener to a signal using [Signal.add] registers the listener
     and gives its ownership to the C code. After attaching it, dropping the
     handle on a listener will not free the listener and its associated
     resources: one needs to explicitly call [detach] first (which un-registers
     it from the signal).

     NB: Detaching a listener then re-attaching it to the same or a different
     signal is possible -- detaching a listener does not necessarily means
     destroying it *)
  type 'a t
  val compare : 'a t -> 'a t -> int
  val equal : 'a t -> 'a t -> bool
  val hash : 'a t -> int

  val create : ('a -> unit) -> 'a t
  val state : 'a t -> [`attached | `detached]
  val detach : 'a t -> unit
end

module Signal : sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val equal : 'a t -> 'a t -> bool
  val hash : 'a t -> int

  val add : 'a t -> 'a Listener.t -> unit
end

module Event_loop : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module Display : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val create : unit -> t
  val get_event_loop : t -> Event_loop.t
  val run : t -> unit
  val destroy : t -> unit
end

module Output : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  module Mode : sig
    type t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int

    val flags : t -> Unsigned.uint32
    val width : t -> int32
    val height : t -> int32
    val refresh : t -> int32 (* mHz *)
  end

  val modes : t -> Mode.t list
  val set_mode : t -> Mode.t -> bool
  val make_current : t -> bool
  val swap_buffers : t -> bool
  val create_global : t -> unit

  module Events : sig
    val destroy : t -> t Signal.t
    val frame : t -> t Signal.t
  end
end

module Renderer : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val begin_ : t -> Output.t -> unit
  val end_ : t -> unit
  val clear : t -> float * float * float * float -> unit
end

module Backend : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val autocreate : Display.t -> t
  val start : t -> bool
  val get_renderer : t -> Renderer.t

  module Events : sig
    val new_output : t -> Output.t Signal.t
  end
end
