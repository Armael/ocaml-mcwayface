let prologue = "
#define _POSIX_C_SOURCE 200809L
#include <wayland-server.h>
#include <wlr/backend.h>
#include <wlr/render.h>
"

let () =
  print_endline prologue;
  Cstubs_structs.write_c Format.std_formatter
    (module Bindings_structs_lib.Bindings_structs.Make)
