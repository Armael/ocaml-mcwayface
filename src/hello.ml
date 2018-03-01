open Wlroots
open Wl

type output_data = {
  destroy : Output.t Listener.t;
  frame : Output.t Listener.t;
}

module OH = Hashtbl.Make (Output)

let outputs : output_data OH.t = OH.create 15

let output_destroy_notify output =
  let data = OH.find outputs output in
  OH.remove outputs output;
  Listener.detach data.destroy;
  Listener.detach data.frame

let output_frame_notify backend output =
  let renderer = Backend.get_renderer backend in
  Output.make_current output |> ignore;
  Renderer.begin_ renderer output;
  Renderer.clear renderer (1., 0., 0., 1.);
  Output.swap_buffers output |> ignore;
  Renderer.end_ renderer;
  ()

let new_output_notify backend output =
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
    frame = Listener.create (output_frame_notify backend);
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

  let new_output = Listener.create (new_output_notify backend) in
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

  let _compositor = Compositor.create dpy (Backend.get_renderer backend) in
  let _ = Xdg_shell_v6.create dpy in

  Display.run dpy;
  Display.destroy dpy;
  exit 0
