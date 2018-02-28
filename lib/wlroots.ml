open Ctypes

module Bindings = Wlroots_bindings.Bindings.Make (Ffi_generated)
module Types = Wlroots_bindings.Bindings.Types

open Wlroots_bindings.Bindings.Utils

let ptr_hash : 'a ptr -> int = fun p ->
  to_voidp p |> raw_address_of_ptr |> Hashtbl.hash

let mk_equal compare x y = compare x y = 0

module O = struct
  type 'a data =
    | Owned of 'a
    | Transfered_to_C of unit ptr (* GC root *)

  type 'a t = { mutable box : 'a data }

  let compare cmp x1 x2 =
    match (x1.box, x2.box) with
    | Owned o1, Owned o2 -> cmp o1 o2
    | Transfered_to_C p1, Transfered_to_C p2 -> ptr_compare p1 p2
    | Owned _, Transfered_to_C _ -> -1
    | Transfered_to_C _, Owned _ -> 1

  let hash h x =
    match x.box with
    | Owned o -> h o
    | Transfered_to_C p -> ptr_hash p

  let create : 'a -> 'a t = fun data ->
    { box = Owned data }

  let transfer_ownership_to_c : 'a t -> unit = function
    | { box = Owned data } as t ->
      let root = Root.create data in
      t.box <- Transfered_to_C root
    | { box = Transfered_to_C _ } ->
      failwith "transfer_ownership: data not owned"

  let reclaim_ownership : 'a t -> 'a = function
    | { box = Transfered_to_C root } as t ->
      let data = Root.get root in
      Root.release root;
      t.box <- Owned data;
      data
    | { box = Owned _ } ->
      failwith "reclaim_ownership: data not transfered to C"

  let state : 'a t -> [`owned | `transfered_to_c] = function
    | { box = Owned _ } -> `owned
    | { box = Transfered_to_C _ } -> `transfered_to_c
end

module Event_loop = struct
  type t = unit ptr
  let compare = ptr_compare
  let equal = ptr_eq
  let hash = ptr_hash
end

module Listener = struct
  type 'a listener = {
    c : Types.Wl_listener.t ptr;
    (* Tie the lifetime of the OCaml callback function to the lifetime of the C
       structure, to prevent untimely memory collection *)
    notify : 'a -> unit;
  }

  type 'a t = 'a listener O.t

  let compare x1 x2 = O.compare (fun t1 t2 -> ptr_compare t1.c t2.c) x1 x2
  let equal x y = mk_equal compare x y
  let hash t = O.hash (fun t -> ptr_hash t.c) t

  let create (notify : 'a -> unit) : 'a t =
    let c_struct = make Types.Wl_listener.t in
    (* we do not set the [notify] field of the C structure yet. It will be done
       by [Signal.add], which will provide the coercion function from [void*] to
       ['a], computed from the [typ] field of the signal. *)
    O.create { c = addr c_struct; notify }

  let state (listener : 'a t) : [`attached | `detached] =
    match O.state listener with
    | `owned -> `detached
    | `transfered_to_c -> `attached

  let detach (listener : 'a t) =
    match O.state listener with
    | `owned -> ()
    | `transfered_to_c ->
      let raw_listener = O.reclaim_ownership listener in
      (* Detach the listener from its signal, as advised in the documentation of
         [wl_listener]. *)
      Bindings.wl_list_remove (raw_listener.c |-> Types.Wl_listener.link)
end

module Signal = struct
  type 'a t = {
    c : Types.Wl_signal.t ptr;
    typ : 'a typ;
  }

  let compare t1 t2 = ptr_compare t1.c t2.c
  let equal x y = mk_equal compare x y
  let hash t = ptr_hash t.c

  let add (signal : 'a t) (listener : 'a Listener.t) =
    match listener with
    | O.{ box = Owned raw_listener } ->
      setf (!@ (raw_listener.c)) Types.Wl_listener.notify
        (fun _ data -> raw_listener.notify (coerce (ptr void) signal.typ data));
      Bindings.wl_signal_add signal.c raw_listener.c;
      O.transfer_ownership_to_c listener
    | O.{ box = Transfered_to_C _ } ->
      failwith "Signal.add: cannot attach the same listener to multiple signals"
end

module Display = struct
  type t = unit ptr

  let compare = ptr_compare
  let equal = mk_equal compare
  let hash = ptr_hash

  let create () =
    let dpy = Bindings.wl_display_create () in
    if is_null dpy then failwith "Display.create";
    dpy

  let get_event_loop dpy =
    let el = Bindings.wl_display_get_event_loop dpy in
    if is_null el then failwith "Display.get_event_loop";
    el

  let run = Bindings.wl_display_run
  let destroy = Bindings.wl_display_destroy
end

module Output = struct
  type t = Types.Output.t ptr
  let t = ptr Types.Output.t

  let compare = ptr_compare
  let equal = mk_equal compare
  let hash = ptr_hash

  module Mode = struct
    type t = Types.Output_mode.t ptr

    let compare = ptr_compare
    let equal = mk_equal compare
    let hash = ptr_hash

    let flags mode = mode |->> Types.Output_mode.flags
    let width mode = mode |->> Types.Output_mode.width
    let height mode = mode |->> Types.Output_mode.height
    let refresh mode = mode |->> Types.Output_mode.refresh
  end

  let modes (output : t) : Mode.t list =
    (output |-> Types.Output.modes)
    |> Bindings.ocaml_of_wl_list
      (fun p -> container_of p Types.Output_mode.link Types.Output_mode.t)

  let set_mode (output : t) (mode : Mode.t) =
    Bindings.wlr_output_set_mode output mode

  let make_current (output : t) : bool =
    (* TODO: handle buffer age *)
    Bindings.wlr_output_make_current output
      (coerce (ptr void) (ptr int) null)

  let swap_buffers (output : t) : bool =
    Bindings.wlr_output_swap_buffers output null null

  let create_global (output : t) =
    Bindings.wlr_output_create_global output

  module Events = struct
    let destroy (output : t) : t Signal.t = {
      c = output |-> Types.Output.events_destroy;
      typ = t;
    }

    let frame (output : t) : t Signal.t = {
      c = output |-> Types.Output.events_frame;
      typ = t;
    }
  end
end

module Renderer = struct
  type t = Types.Renderer.t ptr

  let compare = ptr_compare
  let equal = mk_equal compare
  let hash = ptr_hash

  let begin_ (renderer : t) (output : Output.t) =
    Bindings.wlr_renderer_begin renderer output

  let end_ (renderer : t) =
    Bindings.wlr_renderer_end renderer

  let clear (renderer : t) ((c1,c2,c3,c4) : float * float * float * float) =
    let color_arr = CArray.of_list float [c1;c2;c3;c4] in
    Bindings.wlr_renderer_clear renderer (CArray.start color_arr)
end

module Backend = struct
  type t = Types.Backend.t ptr

  let compare = ptr_compare
  let equal = mk_equal compare
  let hash = ptr_hash

  let autocreate dpy =
    let b = Bindings.wlr_backend_autocreate dpy in
    if is_null b then failwith "Backend.autocreate";
    b

  let start = Bindings.wlr_backend_start

  let get_renderer = Bindings.wlr_backend_get_renderer

  module Events = struct
    let new_output (backend : t) : Output.t Signal.t = {
      c = backend |-> Types.Backend.events_new_output;
      typ = Output.t;
    }
  end
end
