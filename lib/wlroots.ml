module Types = Wlroots_bindings.Ffi_bindings.Types(Ffi_generated_types)
module Bindings = Wlroots_bindings.Ffi_bindings.Bindings(Ffi_generated)

module Display = struct
  type t = unit Ctypes.ptr
  let create () = Bindings.wl_display_create ()
end
