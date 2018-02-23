module Bindings = Wlroots_bindings.Bindings.Make (Ffi_generated)

module Display = struct
  type t = unit Ctypes.ptr
  let create () = Bindings.wl_display_create ()
end
