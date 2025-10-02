(*Word Frequency Counter*)
(*Sean Mackay*)

(* Tokenize a string into words using simple whitespace splitting *)
let tokenize (line : string) : string list =
  String.split_on_char ' ' line
  |> List.filter (fun word -> String.trim word <> "")

(* Read lines from a file and return as a list of strings *)
let read_lines (filename : string) : string list =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      lines := line :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* Count word frequencies using a hashtable *)
let count_words (lines : string list) : (string, int) Hashtbl.t =
  let table = Hashtbl.create 100 in
  List.iter (fun line ->
    let words = tokenize line in
    List.iter (fun word ->
      let word = String.lowercase_ascii word in
      Hashtbl.replace table word ((Hashtbl.find_opt table word |> Option.value ~default:0) + 1)
    ) words
  ) lines;
  table

(* Print the word counts sorted alphabetically *)
let print_word_counts (table : (string, int) Hashtbl.t) : unit =
  Hashtbl.to_seq table
  |> List.of_seq
  |> List.sort (fun (w1, _) (w2, _) -> String.compare w1 w2)
  |> List.iter (fun (word, count) ->
    Printf.printf "%s: %d\n" word count
  )

(* Main function *)
let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: ocaml word_count.ml <filename>";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let lines = read_lines filename in
  let table = count_words lines in
  print_word_counts table
