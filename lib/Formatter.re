open Utils;

let compareOpToString = (op: Types.compareOp): string =>
  switch (op) {
  | Equal => "="
  | NotEqual => "<>"
  | GreaterThan => ">"
  | GreaterThanEqual => ">="
  | LessThan => "<"
  | LessThanEqual => "<="
  };

module From = {
  type t = Types.fromBuilder;

  let build = (~prettyPrint=false, f: t): string =>
    switch (f) {
    | RawFrom(x) => x
    | NormalFrom(x) => "FROM " ++ escape(x)
    };
};

module Select = {
  type t = Types.selectBuilder;

  let build = (~prettyPrint=false, s: option(t)): string =>
    switch (s) {
    | Some(x) =>
      switch (x) {
      | RawSelect(y) => y
      | ListSelect(xs) =>
        let delimiter = prettyPrint ? ",\n       " : ", ";
        "SELECT " ++ (xs |> List.map(escape) |> String.concat(delimiter));
      }
    | None => "SELECT *"
    };
};

module Join = {
  type t = Types.joinBuilder;

  let build = (x: t): string =>
    switch (x) {
    | RawJoin(y) => y
    | InnerJoin(tableName, left, right) =>
      "JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
    | LeftJoin(tableName, left, right) =>
      "LEFT JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
    | RightJoin(tableName, left, right) =>
      "RIGHT JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
    };

  let buildAll = (~prettyPrint=false, xs: list(t)): string =>
    xs |> List.rev |> List.map(build) |> join(prettyPrint ? "\n" : " ");
};

module Where = {
  type t = Types.whereBuilder;

  let buildP = (name, x, op) => escape(name) ++ " " ++ compareOpToString(op) ++ " " ++ x;
  let buildIn = (mapper: 'a => string, name: string, xs: list('a)): string =>
    escape(name) ++ " IN (" ++ (xs |> List.map(mapper) |> String.concat(", ")) ++ ")";

  let build = (~prettyPrint=false, ~toSQL, w: t): string =>
    switch (w) {
    | RawWhere(x) => x
    | IntOpWhere(name, x, op) => buildP(name, intForSQL(x), op)
    | FloatOpWhere(name, x, op) => buildP(name, floatForSQL(x), op)
    | StringOpWhere(name, x, op) => buildP(name, stringForSQL(x), op)
    | IsNullWhere(name) => escape(name) ++ " IS NULL"
    | NotNullWhere(name) => escape(name) ++ " IS NOT NULL"
    | IntInWhere(name, xs) => buildIn(intForSQL, name, xs)
    | FloatInWhere(name, xs) => buildIn(floatForSQL, name, xs)
    | StringInWhere(name, xs) => buildIn(stringForSQL, name, xs)
    | ExistsWhere(builder) =>
      "EXISTS ("
      ++ (prettyPrint ? "\n" : "")
      ++ toSQL({...builder, select: Some(RawSelect("SELECT 1"))})
      ++ (prettyPrint ? "\n" : "")
      ++ ")"
    | NotExistsWhere(builder) =>
      "NOT EXISTS ("
      ++ (prettyPrint ? "\n" : "")
      ++ toSQL({...builder, select: Some(RawSelect("SELECT 1"))})
      ++ (prettyPrint ? "\n" : "")
      ++ ")"
    };

  let buildAll = (~prettyPrint=false, ~toSQL, xs: list(t)): string => {
    let delimiter = prettyPrint ? "\nAND " : " AND ";

    xs
    |> List.rev
    |> List.map(build(~toSQL, ~prettyPrint))
    |> (
      fun
      | [] => ""
      | [x] => "WHERE " ++ x
      | [head, ...tail] => "WHERE " ++ head ++ delimiter ++ String.concat(delimiter, tail)
    );
  };
};

let formatFrom = From.build;
let formatSelect = Select.build;
let formatJoins = Join.buildAll;
let formatWheres = Where.buildAll;

let toSelectSQL = (builder: Types.builder, toSQL) => {
  let prettyPrint = builder.prettyPrint;
  let depth = builder.depth;
  let from =
    switch (builder.from) {
    | Some(x) => x
    | None => raise(Invalid_argument("Missing from"))
    };
  [
    formatSelect(~prettyPrint, builder.select),
    formatFrom(~prettyPrint, from),
    formatJoins(~prettyPrint, builder.joins),
    formatWheres(~prettyPrint, ~toSQL, builder.wheres),
  ]
  |> List.filter(x => x != "")
  |> String.concat(prettyPrint ? "\n" ++ String.make(depth * 2, ' ') : " ")
  |> prepend(prettyPrint ? String.make(depth * 2, ' ') : "");
};

let rec toSQL = (builder: Types.builder): string =>
  switch (builder.operation) {
  | Select => toSelectSQL(builder, toSQL)
  | Update => raise(Invalid_argument("Not implemented"))
  | Insert => raise(Invalid_argument("Not implemented"))
  | Destroy => raise(Invalid_argument("Not implemented"))
  };