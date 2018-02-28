open Ctypes

module Types = Bindings_structs_lib.Bindings_structs.Make (Generated_types)

(* TODO: move *)
module Utils = struct
  let ptr_eq p q = Ctypes.ptr_compare p q = 0

  (* should be equivalent to [wl_container_of].

     The arguments differ slightly. We do not need the [sample] argument
     compared to the C macro, since our OCaml representation of pointers &
     fields already carries some type information. On the other hand, we require
     an extra argument [container_typ] carrying the description of the expected
     container type. *)
  let container_of p field container_typ =
    p -@ (offsetof field)
    |> coerce (ptr (field_type field)) (ptr container_typ)

  let ( |->> ) s f = !@ (s |-> f)
end

module Make (F : Cstubs.FOREIGN) =
struct
  open Ctypes
  open F
  open Types
  open Utils

  (* wl_list *)

  type wl_list_p = Wl_list.t ptr
  let wl_list_p = ptr Wl_list.t

  let prev (l: wl_list_p) : wl_list_p = Ctypes.(getf (!@ l) Wl_list.prev)
  let next (l: wl_list_p) : wl_list_p = Ctypes.(getf (!@ l) Wl_list.next)

  let wl_list_init = foreign "wl_list_init"
      (wl_list_p @-> returning void)

  let wl_list_remove = foreign "wl_list_remove"
      (wl_list_p @-> returning void)

  let ocaml_of_wl_list
      (extract_elt : wl_list_p -> 'a)
      (l : wl_list_p) :
    'a list
    =
    let rec aux acc elt =
      if ptr_eq elt l then List.rev acc
      else aux ((extract_elt elt)::acc) (next elt)
    in
    if Ctypes.(ptr_eq (coerce wl_list_p (ptr void) l) null) then []
    else aux [] (next l)

  (* wl_listener *)

  let wl_listener_p = ptr Wl_listener.t

  (* wl_signal *)

  let wl_signal_p = ptr Wl_signal.t

  let wl_signal_add = foreign "wl_signal_add"
      (wl_signal_p @-> wl_listener_p @-> returning void)

  (* wl_event_loop *)

  let wl_event_loop_p = ptr void

  (* wl_display *)

  let wl_display_p = ptr void

  let wl_display_create = foreign "wl_display_create"
      (void @-> returning wl_display_p)

  let wl_display_get_event_loop = foreign "wl_display_get_event_loop"
      (wl_display_p @-> returning wl_event_loop_p)

  let wl_display_run = foreign "wl_display_run"
      (wl_display_p @-> returning void)

  let wl_display_destroy = foreign "wl_display_destroy"
      (wl_display_p @-> returning void)

  let wlr_backend_p = Ctypes.ptr Backend.t

  let wlr_backend_autocreate = foreign "wlr_backend_autocreate"
      (wl_display_p @-> returning wlr_backend_p)

  let wlr_backend_start = foreign "wlr_backend_start"
      (wlr_backend_p @-> returning bool)

  (* wlr_output_mode *)

  let wlr_output_mode_p = ptr Output_mode.t

  (* wlr_output *)

  let wlr_output_p = ptr Output.t

  let wlr_output_set_mode = foreign "wlr_output_set_mode"
      (wlr_output_p @-> wlr_output_mode_p @-> returning bool)

  let wlr_output_make_current = foreign "wlr_output_make_current"
      (wlr_output_p @-> ptr int @-> returning bool)

  (* TODO: handle "when" and "damage" *)
  let wlr_output_swap_buffers = foreign "wlr_output_swap_buffers"
      (wlr_output_p @-> ptr void @-> ptr void @-> returning bool)

  let wlr_output_create_global = foreign "wlr_output_create_global"
      (wlr_output_p @-> returning void)

  (* wlr_renderer *)

  let wlr_renderer_p = ptr Renderer.t

  let wlr_renderer_begin = foreign "wlr_renderer_begin"
      (wlr_renderer_p @-> wlr_output_p @-> returning void)

  let wlr_renderer_end = foreign "wlr_renderer_end"
      (wlr_renderer_p @-> returning void)

  let wlr_renderer_clear = foreign "wlr_renderer_clear"
      (wlr_renderer_p @-> ptr float @-> returning void)

  (* wlr_backend *)

  let wlr_backend_p = ptr Backend.t

  let wlr_backend_get_renderer = foreign "wlr_backend_get_renderer"
      (wlr_backend_p @-> returning wlr_renderer_p)
end
