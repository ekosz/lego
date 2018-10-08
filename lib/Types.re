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

type builder = {
  prettyPrint: bool,
  depth: int,
  select: option(selectBuilder),
  operation: crud,
  from: option(fromBuilder),
  joins: list(joinBuilder),
  wheres: list(whereBuilder),
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
  | IntInWhere(string, list(int))
  | FloatInWhere(string, list(float))
  | StringInWhere(string, list(string))
  | ExistsWhere(builder)
  | NotExistsWhere(builder);