open Ctypes

module Types = Bindings_structs_lib.Bindings_structs.Make (Generated_types)

module Make (F : Cstubs.FOREIGN) =
struct
  open F
  open Types

  let ptr_eq p q = ptr_compare p q = 0

  let wl_display_p = Ctypes.(ptr void)
  let wl_event_loop_p = Ctypes.(ptr void)

  let wl_display_create = foreign "wl_display_create"
      (void @-> returning wl_display_p)
end
