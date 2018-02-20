module Types (F : Cstubs.Types.TYPE) =
struct

end

module Bindings (F : Cstubs.FOREIGN) =
struct
  let foreign = F.foreign

  module Foreign = struct
    include Ctypes
    include Foreign
  end
  module Ctypes = struct
    include Ctypes

    let (@->)         = F.(@->)
    let returning     = F.returning
    let foreign       = F.foreign
    let foreign_value = F.foreign_value
  end

  let wl_display_p = Ctypes.(ptr void)

  let wl_display_create = foreign "wl_display_create"
      Ctypes.(void @-> returning wl_display_p)
end
