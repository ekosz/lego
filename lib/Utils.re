let append = (toAppend: string, body: string): string => body ++ toAppend;
let join = String.concat;
let commaJoin = join(", ");
let newlineJoin = join("\n");

let rec escape = (x: string) =>
  x
  |> String.trim
  |> String.split_on_char('.')
  |> (
    fun
    /* split_on_char should never get here */
    | [] => raise(Not_found)
    | [x] => "\"" ++ x ++ "\""
    | arr => arr |> List.map(escape) |> join(".")
  );