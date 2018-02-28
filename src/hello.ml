open Wlroots

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
  begin match Output.modes output with
    | mode :: _ -> ignore (Output.set_mode output mode)
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
  let dpy = Display.create () in
  let _event_loop = Display.get_event_loop dpy in
  let backend = Backend.autocreate dpy in

  let new_output = Listener.create (new_output_notify backend) in
  Signal.add (Backend.Events.new_output backend) new_output;

  if not (Backend.start backend) then (
    Format.(fprintf err_formatter "Failed to start backend\n");
    Display.destroy dpy;
    exit 1
  );

  Display.run dpy;
  Display.destroy dpy;
  exit 0
