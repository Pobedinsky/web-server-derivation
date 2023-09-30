open Unix

let read_command_output command =
  let in_channel = open_process_in command in
  let rec read_lines accum =
    try
      let line = input_line in_channel in
      read_lines (line :: accum)
    with End_of_file ->
      close_in in_channel;
      List.rev accum
  in
  let output_lines = read_lines [] in
  String.concat "\n" output_lines (* You can process the output here *)

let compute str =
  let command = "echo "^str^" | ../ocaml_unix/demo" in  (* Replace with your desired shell command *)
  read_command_output command

let count = ref 0

let count_requests inner_handler request =
  count := !count + 1;
  inner_handler request

let () =
  Dream.run
  @@ Dream.logger
  @@ count_requests
  @@ Dream.router [
    Dream.get "/derive/:str" (fun request ->
      Dream.html ((Dream.param request "str") |> compute));
  ]
