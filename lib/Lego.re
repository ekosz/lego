type t = Types.builder;

let lego = (~tableName=?, ~depth=0, ~prettyPrint=false, ()): t => {
  depth,
  prettyPrint,
  from:
    switch (tableName) {
    | Some(t) => Some(NormalFrom(t))
    | None => None
    },
  limit: None,
  offset: None,
  operation: Select,
  select: None,
  ctes: [],
  joins: [],
  wheres: [],
  orders: [],
  groupBys: [],
  unions: [],
};

let ss /* set select */ = (b: t, x) => {...b, operation: Select, select: Some(x)};
let select = (ls, builder: t) => ss(builder, ListSelect(ls));
let selectStar = (builder: t) => ss(builder, RawSelect("*"));
let selectRaw = (rawString, builder: t) => ss(builder, RawSelect(rawString));

let sf /* set from */ = (b: t, x) => {...b, from: Some(x)};
let from = (tableName, builder: t) => sf(builder, NormalFrom(tableName));
let fromRaw = (rawString, builder: t) => sf(builder, RawFrom(rawString));

let aj /* append join */ = (b: t, x) => {...b, joins: [x, ...b.joins]};
let joinRaw = (rawString, builder: t) => aj(builder, RawJoin(rawString));
let join = (tableName, left, right, builder: t) =>
  aj(builder, InnerJoin(tableName, left, right));
let leftJoin = (tableName, left, right, builder: t) =>
  aj(builder, LeftJoin(tableName, left, right));
let rightJoin = (tableName, left, right, builder: t) =>
  aj(builder, RightJoin(tableName, left, right));

let aw /* append where */ = (b: t, x) => {...b, wheres: [x, ...b.wheres]};
let whereRaw = (rawString, builder: t) => aw(builder, RawWhere(rawString));
let whereInt = (name, op, x, builder: t) => aw(builder, IntOpWhere(name, x, op));
let whereFloat = (name, op, x, builder: t) => aw(builder, FloatOpWhere(name, x, op));
let whereString = (name, op, x, builder: t) => aw(builder, StringOpWhere(name, x, op));
let whereNull = (name, builder: t) => aw(builder, IsNullWhere(name));
let whereNotNull = (name, builder: t) => aw(builder, NotNullWhere(name));
let whereTrue = (name, builder: t) => aw(builder, IsTrueWhere(name));
let whereFalse = (name, builder: t) => aw(builder, IsFalseWhere(name));
let whereStringIn = (name, xs, builder: t) => aw(builder, StringInWhere(name, xs));
let whereIntIn = (name, xs, builder: t) => aw(builder, IntInWhere(name, xs));
let whereFloatIn = (name, xs, builder: t) => aw(builder, FloatInWhere(name, xs));
let whereInSub = (name, cb, builder: t) => {
  let subBuilder = cb(lego(~depth=builder.depth + 1, ()));
  aw(builder, SubInWhere(name, subBuilder));
};
let whereExists = (cb, builder: t) => {
  let subBuilder = cb(lego(~depth=builder.depth + 1, ()));
  aw(builder, ExistsWhere(subBuilder));
};
let whereNotExists = (cb, builder: t) => {
  let subBuilder = cb(lego(~depth=builder.depth + 1, ()));
  aw(builder, NotExistsWhere(subBuilder));
};

let ao /* append order */ = (b: t, x) => {...b, orders: [x, ...b.orders]};
let orderRaw = (rawString, builder: t) => ao(builder, RawOrder(rawString));
let order = (column, direction, builder: t) => ao(builder, NormalOrder(column, direction));

let ag /* append group */ = (b: t, x) => {...b, groupBys: [x, ...b.groupBys]};
let groupByRaw = (rawString, builder: t) => ag(builder, RawGroupBy(rawString));
let groupBy = (column, builder: t) => ag(builder, NormalGroupBy(column));

let limit = (limit, builder: t) => {...builder, limit: Some(limit)};
let offset = (offset, builder: t) => {...builder, offset: Some(offset)};

let toSQL = Formatter.toSQL;