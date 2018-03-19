open Wlroots
open Wl

(* global state *)

type output_data = {
  destroy : Output.t Listener.t;
  frame : Output.t Listener.t;
}

module OH = Hashtbl.Make (Output)

let outputs : output_data OH.t = OH.create 15

(* ---- *)

let output_destroy_notify output =
  let data = OH.find outputs output in
  OH.remove outputs output;
  Listener.detach data.destroy;
  Listener.detach data.frame

let output_frame_notify comp backend output =
  let renderer = Backend.get_renderer backend in
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

let new_output_notify comp backend output =
  let modes = Output.modes output in
  Printf.printf "Found %d output modes\n%!" (List.length modes);
  begin match modes with
    | mode :: _ ->
      Printf.printf "Set mode: %ldx%ld\n%!"
        (Output.Mode.width mode) (Output.Mode.height mode);
      ignore (Output.set_mode output mode)
    | _ -> ()
  end;

  let output_data = {
    destroy = Listener.create output_destroy_notify;
    frame = Listener.create (output_frame_notify comp backend);
  }
  in
  OH.add outputs output output_data;
  Signal.add (Output.Events.destroy output) output_data.destroy;
  Signal.add (Output.Events.frame output) output_data.frame;
  Output.create_global output;
  ()

let () =
  Log.(init Debug);
  let dpy = Display.create () in
  let _event_loop = Display.get_event_loop dpy in
  let backend = Backend.autocreate dpy in

  let comp = Compositor.create dpy (Backend.get_renderer backend) in
  let new_output = Listener.create (new_output_notify comp backend) in
  Signal.add (Backend.Events.new_output backend) new_output;

  let socket = Display.add_socket_auto dpy in

  if not (Backend.start backend) then (
    Format.(fprintf err_formatter "Failed to start backend\n");
    Display.destroy dpy;
    exit 1
  );

  Printf.printf "Running compositor on wayland display '%s'\n%!" socket;
  Unix.putenv "WAYLAND_DISPLAY" socket;

  let _ = Display.init_shm dpy in
  let _ = Gamma_control.Manager.create dpy in
  let _ = Screenshooter.create dpy in
  let _ = Primary_selection.Device_manager.create dpy in
  let _ = Idle.create dpy in

  let _ = Xdg_shell_v6.create dpy in

  Display.run dpy;
  Display.destroy dpy;
  exit 0
