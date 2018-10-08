open Utils;

type crud =
  | Select
  | Update
  | Insert
  | Destroy;

type compareOp =
  | Equal
  | NotEqual
  | GreaterThan
  | GreaterThanEqual
  | LessThan
  | LessThanEqual;

let compareOpToString = (op: compareOp): string =>
  switch (op) {
  | Equal => "="
  | NotEqual => "<>"
  | GreaterThan => ">"
  | GreaterThanEqual => ">="
  | LessThan => "<"
  | LessThanEqual => "<="
  };

module FromBuilder = {
  type t =
    | Raw(string)
    | Normal(string);

  let build = (~prettyPrint=false, f: t): string =>
    switch (f) {
    | Raw(x) => x
    | Normal(x) => "FROM " ++ escape(x)
    };
};

module SelectBuilder = {
  type t =
    | Raw(string)
    | List(list(string));

  let build = (~prettyPrint=false, s: option(t)): string =>
    switch (s) {
    | Some(x) =>
      switch (x) {
      | Raw(y) => y
      | List(xs) =>
        let delimiter = prettyPrint ? ",\n       " : ", ";
        "SELECT " ++ (xs |> List.map(escape) |> String.concat(delimiter));
      }
    | None => "SELECT *"
    };
};

module JoinBuilder = {
  type t =
    | Raw(string)
    | Inner(string, string, string)
    | Left(string, string, string)
    | Right(string, string, string);

  let build = (x: t): string =>
    switch (x) {
    | Raw(y) => y
    | Inner(tableName, left, right) =>
      "JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
    | Left(tableName, left, right) =>
      "LEFT JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
    | Right(tableName, left, right) =>
      "RIGHT JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
    };

  let buildAll = (~prettyPrint=false, xs: list(t)): string =>
    xs |> List.rev |> List.map(build) |> join(prettyPrint ? "\n" : " ");
};

module WhereBuilder = {
  type t =
    | Raw(string)
    | IntOp(string, int, compareOp)
    | FloatOp(string, float, compareOp)
    | StringOp(string, string, compareOp);

  let buildP = (name, x, op) => escape(name) ++ " " ++ compareOpToString(op) ++ " " ++ x;

  let build = (w: t): string =>
    switch (w) {
    | Raw(x) => x
    | IntOp(name, x, op) => buildP(name, string_of_int(x), op)
    | FloatOp(name, x, op) =>
      /* We need to add a check here for floats like 4. and turn them into 4.0 */
      let floatString = string_of_float(x);
      let finalString =
        floatString.[String.length(floatString) - 1] == '.' ? floatString ++ "0" : floatString;

      buildP(name, finalString, op);
    | StringOp(name, x, op) => buildP(name, "'" ++ x ++ "'", op)
    };

  let buildAll = (~prettyPrint=false, xs: list(t)): string => {
    let delimiter = prettyPrint ? "\nAND " : " AND ";

    xs
    |> List.rev
    |> List.map(build)
    |> (
      fun
      | [] => ""
      | [x] => "WHERE " ++ x
      | [head, ...tail] => "WHERE " ++ head ++ delimiter ++ String.concat(delimiter, tail)
    );
  };
};

type t = {
  prettyPrint: bool,
  select: option(SelectBuilder.t),
  operation: crud,
  from: option(FromBuilder.t),
  joins: list(JoinBuilder.t),
  wheres: list(WhereBuilder.t),
};

let lego = (~tableName=?, ~prettyPrint=false, ()) => {
  prettyPrint,
  from:
    switch (tableName) {
    | Some(t) => Some(FromBuilder.Normal(t))
    | None => None
    },
  operation: Select,
  select: None,
  joins: [],
  wheres: [],
};

let select = (ls, builder) => {...builder, select: Some(SelectBuilder.List(ls))};
let selectStar = builder => {
  ...builder,
  operation: Select,
  select: Some(SelectBuilder.Raw("*")),
};
let selectRaw = (rawString, builder) => {
  ...builder,
  operation: Select,
  select: Some(SelectBuilder.Raw(rawString)),
};

let from = (tableName, builder) => {...builder, from: Some(FromBuilder.Normal(tableName))};
let fromRaw = (rawString, builder) => {...builder, from: Some(FromBuilder.Raw(rawString))};

let joinRaw = (rawString, builder) => {
  ...builder,
  joins: [JoinBuilder.Raw(rawString), ...builder.joins],
};
let join = (tableName, left, right, builder) => {
  ...builder,
  joins: [JoinBuilder.Inner(tableName, left, right), ...builder.joins],
};
let leftJoin = (tableName, left, right, builder) => {
  ...builder,
  joins: [JoinBuilder.Left(tableName, left, right), ...builder.joins],
};
let rightJoin = (tableName, left, right, builder) => {
  ...builder,
  joins: [JoinBuilder.Right(tableName, left, right), ...builder.joins],
};

let whereRaw = (rawString, builder) => {
  ...builder,
  wheres: [WhereBuilder.Raw(rawString), ...builder.wheres],
};
let whereInt = (name, op, x, builder) => {
  ...builder,
  wheres: [WhereBuilder.IntOp(name, x, op), ...builder.wheres],
};
let whereFloat = (name, op, x, builder) => {
  ...builder,
  wheres: [WhereBuilder.FloatOp(name, x, op), ...builder.wheres],
};
let whereString = (name, op, x, builder) => {
  ...builder,
  wheres: [WhereBuilder.StringOp(name, x, op), ...builder.wheres],
};

let toSelectSQL = builder => {
  let {prettyPrint} = builder;
  let from =
    switch (builder.from) {
    | Some(x) => x
    | None => raise(Invalid_argument("Missing from"))
    };
  [
    SelectBuilder.build(~prettyPrint, builder.select),
    FromBuilder.build(~prettyPrint, from),
    JoinBuilder.buildAll(~prettyPrint, builder.joins),
    WhereBuilder.buildAll(~prettyPrint, builder.wheres),
  ]
  |> List.filter(x => x != "")
  |> String.concat(prettyPrint ? "\n" : " ")
  |> append(";");
};

let toSQL = (builder: t): string =>
  switch (builder.operation) {
  | Select => toSelectSQL(builder)
  | Update => raise(Invalid_argument("Not implemented"))
  | Insert => raise(Invalid_argument("Not implemented"))
  | Destroy => raise(Invalid_argument("Not implemented"))
  };