let append = (toAppend: string, body: string): string => body ++ toAppend;
let join = String.concat;
let commaJoin = join(", ");
let newlineJoin = join("\n");

/* TODO: Make this escape the string as well */
let stringForSQL = (body: string): string => "'" ++ body ++ "'";
let floatForSQL = (num: float): string => {
  /* We need to add a check here for floats like 4. and turn them into 4.0 */
  let floatString = string_of_float(num);
  floatString.[String.length(floatString) - 1] == '.' ? floatString ++ "0" : floatString;
};
let intForSQL = string_of_int;

/* TODO: Make this escape bad input */
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