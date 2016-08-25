open Csml_iface
open Winforms

let run () =
  let f = create_form () in
  f # set_text "CSML Winforms example";
  let add c = f # add_control (c :> control) in

  let n = ref 0 in

  let l = create_label () in
  l # set_text "0";
  l # set_location 10 60;
  let set i = n := i; l # set_text (string_of_int i) in
  l # add_click_handler (fun () -> set 0);
  add l;

  let b = create_button () in
  b # set_text "Increment";
  b # add_click_handler (fun () -> set (!n + 1));
  add b;

  let b = create_button () in
  b # set_location 10 30;
  b # set_text "Open file";
  b # add_click_handler
    (fun () ->
      match OpenFileDialog.run ~title:"Choose a file" () with
      | Some f -> MessageBox.show f
      | None -> ());
  add b;

  (* This is to illustrate the propagate of exceptions from C# to OCaml *)
  let ff = create_form () in
  begin
    try add ff
    with Csharp_exception (e, msg, _) as exn ->
      Printf.printf "Got exception, as expected: %S, %S\n%!" e msg;
      Printf.printf "***** Stack trace:\n%s\n%!" (Exn.stack_trace exn);
      Printf.printf "***** Source: %s\n%!" (Exn.source exn);
  end;

  f # show_dialog
