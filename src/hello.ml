open Wlroots
open Wl

let output_frame comp output () =
  let renderer = Compositor.renderer comp in
  Output.make_current output |> ignore;
  Renderer.begin_ renderer output;

  Renderer.clear renderer (0.4, 0.4, 0.4, 1.);

  List.iter (fun resource ->
    let surface = Surface.from_resource resource in
    if Surface.has_buffer surface then (
      let surface_state = Surface.current surface in
      let render_box = Box.{
        x = 20; y = 20;
        width = Surface.State.width surface_state;
        height = Surface.State.height surface_state;
      } in
      let matrix = Matrix.project_box render_box
          (Surface.State.transform surface_state) 0.
          (Output.transform_matrix output)
      in
      let _ : bool = Renderer.render_with_matrix renderer
          (Surface.texture surface) matrix 1.0 in
      Surface.send_frame_done surface (Mtime_clock.now ())
    )
  ) (Compositor.surfaces comp);

  Output.swap_buffers output |> ignore;
  Renderer.end_ renderer;
  ()

let output_added comp output () =
  begin match Output.best_mode output with
    | Some mode ->
      Printf.printf "Set mode: %ldx%ld\n%!"
        (Output.Mode.width mode) (Output.Mode.height mode);
      ignore (Output.set_mode output mode)
    | None -> ()
  end;
  Output.create_global output

let () =
  Log.(init Debug);
  let comp = Compositor.create () in
  Main.run ~state:() ~handler:(function
    | Compositor.New_output o -> output_added comp o
    | Output.Frame o -> output_frame comp o
    | _ -> fun _ -> ()
  ) comp;
  Main.terminate comp;
  exit 0
