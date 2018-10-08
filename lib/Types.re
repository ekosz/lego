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

type orderDirection =
  | ASC
  | DESC;

type builder = {
  prettyPrint: bool,
  depth: int,
  select: option(selectBuilder),
  limit: option(int),
  offset: option(int),
  operation: crud,
  ctes: list(cteBuilder),
  from: option(fromBuilder),
  joins: list(joinBuilder),
  wheres: list(whereBuilder),
  orders: list(orderBuilder),
  groupBys: list(groupByBuilder),
  unions: list(unionBuilder),
}
and selectBuilder =
  | RawSelect(string)
  | ListSelect(list(string))
and fromBuilder =
  | RawFrom(string)
  | NormalFrom(string)
and joinBuilder =
  | RawJoin(string)
  | InnerJoin(string, string, string)
  | LeftJoin(string, string, string)
  | RightJoin(string, string, string)
and whereBuilder =
  | RawWhere(string)
  | IntOpWhere(string, int, compareOp)
  | FloatOpWhere(string, float, compareOp)
  | StringOpWhere(string, string, compareOp)
  | IsNullWhere(string)
  | NotNullWhere(string)
  | IsTrueWhere(string)
  | IsFalseWhere(string)
  | IntInWhere(string, list(int))
  | FloatInWhere(string, list(float))
  | StringInWhere(string, list(string))
  | SubInWhere(string, builder)
  | ExistsWhere(builder)
  | NotExistsWhere(builder)
and orderBuilder =
  | RawOrder(string)
  | NormalOrder(string, orderDirection)
and groupByBuilder =
  | RawGroupBy(string)
  | NormalGroupBy(string)
and cteBuilder =
  | RawCTE(string)
  | NormalCTE(string, builder)
and unionBuilder =
  | Union(builder)
  | UnionAll(builder);