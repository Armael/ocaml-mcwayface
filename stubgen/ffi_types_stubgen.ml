module Ffi_bindings = Wlroots_bindings.Ffi_bindings

let prefix = "wlroots_stub"

let prologue = "
#define _POSIX_C_SOURCE 200809L
#include <wayland-server.h>
#include <wlr/backend.h>
#include <wlr/render.h>
"

let () =
  print_endline prologue;
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
