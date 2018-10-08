open Utils;

type t = Types.builder;

let lego = (~tableName=?, ~depth=0, ~prettyPrint=false, ()): t => {
  depth,
  prettyPrint,
  from:
    switch (tableName) {
    | Some(t) => Some(NormalFrom(t))
    | None => None
    },
  operation: Select,
  select: None,
  joins: [],
  wheres: [],
};

let select = (ls, builder: t) => {...builder, select: Some(ListSelect(ls))};
let selectStar = (builder: t) => {...builder, operation: Select, select: Some(RawSelect("*"))};
let selectRaw = (rawString, builder: t) => {
  ...builder,
  operation: Select,
  select: Some(RawSelect(rawString)),
};

let from = (tableName, builder: t) => {...builder, from: Some(NormalFrom(tableName))};
let fromRaw = (rawString, builder: t) => {...builder, from: Some(RawFrom(rawString))};

let joinRaw = (rawString, builder: t) => {
  ...builder,
  joins: [RawJoin(rawString), ...builder.joins],
};
let join = (tableName, left, right, builder: t) => {
  ...builder,
  joins: [InnerJoin(tableName, left, right), ...builder.joins],
};
let leftJoin = (tableName, left, right, builder: t) => {
  ...builder,
  joins: [LeftJoin(tableName, left, right), ...builder.joins],
};
let rightJoin = (tableName, left, right, builder: t) => {
  ...builder,
  joins: [RightJoin(tableName, left, right), ...builder.joins],
};

let whereRaw = (rawString, builder: t) => {
  ...builder,
  wheres: [RawWhere(rawString), ...builder.wheres],
};
let whereInt = (name, op, x, builder: t) => {
  ...builder,
  wheres: [IntOpWhere(name, x, op), ...builder.wheres],
};
let whereFloat = (name, op, x, builder: t) => {
  ...builder,
  wheres: [FloatOpWhere(name, x, op), ...builder.wheres],
};
let whereString = (name, op, x, builder: t) => {
  ...builder,
  wheres: [StringOpWhere(name, x, op), ...builder.wheres],
};
let whereNull = (name, builder: t) => {
  ...builder,
  wheres: [IsNullWhere(name), ...builder.wheres],
};
let whereNotNull = (name, builder: t) => {
  ...builder,
  wheres: [NotNullWhere(name), ...builder.wheres],
};
let whereStringIn = (name, xs, builder: t) => {
  ...builder,
  wheres: [StringInWhere(name, xs), ...builder.wheres],
};
let whereIntIn = (name, xs, builder: t) => {
  ...builder,
  wheres: [IntInWhere(name, xs), ...builder.wheres],
};
let whereFloatIn = (name, xs, builder: t) => {
  ...builder,
  wheres: [FloatInWhere(name, xs), ...builder.wheres],
};
let whereExists = (cb, builder: t) => {
  let subBuilder = cb(lego(~prettyPrint=builder.prettyPrint, ~depth=builder.depth + 1, ()));
  {...builder, wheres: [ExistsWhere(subBuilder), ...builder.wheres]};
};
let whereNotExists = (cb, builder: t) => {
  let subBuilder = cb(lego(~prettyPrint=builder.prettyPrint, ~depth=builder.depth + 1, ()));
  {...builder, wheres: [NotExistsWhere(subBuilder), ...builder.wheres]};
};

let toSQL = Formatter.toSQL;