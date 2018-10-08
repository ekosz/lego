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

let orderDirectionToString = (d: Types.orderDirection): string =>
  switch (d) {
  | ASC => "ASC"
  | DESC => "DESC"
  };

let formatFrom = (~prettyPrint=false, f: Types.fromBuilder): string =>
  switch (f) {
  | RawFrom(x) => x
  | NormalFrom(x) => "FROM " ++ escape(x)
  };

let formatSelect = (~prettyPrint=false, s: option(Types.selectBuilder)): string =>
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

let formatJoin = (x: Types.joinBuilder): string =>
  switch (x) {
  | RawJoin(y) => y
  | InnerJoin(tableName, left, right) =>
    "JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
  | LeftJoin(tableName, left, right) =>
    "LEFT JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
  | RightJoin(tableName, left, right) =>
    "RIGHT JOIN " ++ escape(tableName) ++ " ON " ++ escape(left) ++ " = " ++ escape(right)
  };

let formatJoins = (~prettyPrint=false, xs: list(Types.joinBuilder)): string =>
  xs |> List.rev |> List.map(formatJoin) |> join(prettyPrint ? "\n" : " ");

let formatOp = (name, x, op) => escape(name) ++ " " ++ compareOpToString(op) ++ " " ++ x;
let formatInOp = (mapper: 'a => string, name: string, xs: list('a)): string =>
  escape(name) ++ " IN (" ++ (xs |> List.map(mapper) |> String.concat(", ")) ++ ")";
let formatWhere = (~prettyPrint=false, ~toSQL, w: Types.whereBuilder): string =>
  switch (w) {
  | RawWhere(x) => x
  | IntOpWhere(name, x, op) => formatOp(name, intForSQL(x), op)
  | FloatOpWhere(name, x, op) => formatOp(name, floatForSQL(x), op)
  | StringOpWhere(name, x, op) => formatOp(name, stringForSQL(x), op)
  | IsNullWhere(name) => escape(name) ++ " IS NULL"
  | NotNullWhere(name) => escape(name) ++ " IS NOT NULL"
  | IsTrueWhere(name) => escape(name) ++ " IS TRUE"
  | IsFalseWhere(name) => escape(name) ++ " IS FALSE"
  | IntInWhere(name, xs) => formatInOp(intForSQL, name, xs)
  | FloatInWhere(name, xs) => formatInOp(floatForSQL, name, xs)
  | StringInWhere(name, xs) => formatInOp(stringForSQL, name, xs)
  | SubInWhere(name, builder) =>
    escape(name)
    ++ " IN ("
    ++ (prettyPrint ? "\n" : "")
    ++ toSQL({...builder, prettyPrint})
    ++ (prettyPrint ? "\n" : "")
    ++ ")"
  | ExistsWhere(builder) =>
    "EXISTS ("
    ++ (prettyPrint ? "\n" : "")
    ++ toSQL({...builder, prettyPrint, select: Some(RawSelect("SELECT 1"))})
    ++ (prettyPrint ? "\n" : "")
    ++ ")"
  | NotExistsWhere(builder) =>
    "NOT EXISTS ("
    ++ (prettyPrint ? "\n" : "")
    ++ toSQL({...builder, prettyPrint, select: Some(RawSelect("SELECT 1"))})
    ++ (prettyPrint ? "\n" : "")
    ++ ")"
  };
let formatWheres = (~prettyPrint=false, ~toSQL, xs: list(Types.whereBuilder)): string => {
  let delimiter = prettyPrint ? "\nAND " : " AND ";

  xs
  |> List.rev
  |> List.map(formatWhere(~toSQL, ~prettyPrint))
  |> (
    fun
    | [] => ""
    | [x] => "WHERE " ++ x
    | [head, ...tail] => "WHERE " ++ head ++ delimiter ++ String.concat(delimiter, tail)
  );
};

let formatGroupBy = (g: Types.groupByBuilder) =>
  switch (g) {
  | RawGroupBy(x) => x
  | NormalGroupBy(column) => escape(column)
  };
let formatGroupBys = (~prettyPrint, xs: list(Types.groupByBuilder)) =>
  switch (xs) {
  | [] => ""
  | rest => "GROUP BY " ++ (xs |> List.map(formatGroupBy) |> commaJoin)
  };

let formatOrder = (~prettyPrint, o: Types.orderBuilder) =>
  switch (o) {
  | RawOrder(x) => x
  | NormalOrder(name, direction) => escape(name) ++ " " ++ orderDirectionToString(direction)
  };
let formatOrders = (~prettyPrint, xs: list(Types.orderBuilder)) => {
  let delimiter = ", ";
  xs
  |> List.rev
  |> List.map(formatOrder(~prettyPrint))
  |> (
    fun
    | [] => ""
    | [x] => "ORDER BY " ++ x
    | [head, ...tail] => "ORDER BY " ++ head ++ delimiter ++ String.concat(delimiter, tail)
  );
};

let formatLimit = (~prettyPrint, limit: option(int)) =>
  switch (limit) {
  | None => ""
  | Some(x) => "LIMIT " ++ intForSQL(x)
  };

let formatOffset = (~prettyPrint, offset: option(int)) =>
  switch (offset) {
  | None => ""
  | Some(x) => "OFFSET " ++ intForSQL(x)
  };

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
    formatGroupBys(~prettyPrint, builder.groupBys),
    formatOrders(~prettyPrint, builder.orders),
    formatLimit(~prettyPrint, builder.limit),
    formatOffset(~prettyPrint, builder.offset),
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