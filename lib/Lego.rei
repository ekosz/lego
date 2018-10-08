type t = Types.builder;

/*** Creates a new builder */
let lego: (~tableName: string=?, ~depth: int=?, ~prettyPrint: bool=?, unit) => t;

/*** Transforms the builder to a SQL string */
let toSQL: t => string;

/*** Adds a column to the selects */
let select: (string, t) => t;
/*** Changes the selection to * */
let selectStar: t => t;
/*** Adds a list of columns to the selects */
let selectList: (list(string), t) => t;
/*** Adds a raw string to list of selects */
let selectRaw: (string, t) => t;

let from: (string, t) => t;
let fromRaw: (string, t) => t;

let joinRaw: (string, t) => t;
let join: (string, string, string, t) => t;
let leftJoin: (string, string, string, t) => t;
let rightJoin: (string, string, string, t) => t;

let whereRaw: (string, t) => t;
let whereInt: (string, Types.compareOp, int, t) => t;
let whereFloat: (string, Types.compareOp, float, t) => t;
let whereString: (string, Types.compareOp, string, t) => t;
let whereNull: (string, t) => t;
let whereNotNull: (string, t) => t;
let whereTrue: (string, t) => t;
let whereFalse: (string, t) => t;
let whereStringIn: (string, list(string), t) => t;
let whereIntIn: (string, list(int), t) => t;
let whereFloatIn: (string, list(float), t) => t;
let whereInSub: (string, t => t, t) => t;
let whereSub: (string, Types.compareOp, t => t, t) => t;
let whereExists: (t => t, t) => t;
let whereNotExists: (t => t, t) => t;

let orderRaw: (string, t) => t;
let order: (string, Types.orderDirection, t) => t;

let groupByRaw: (string, t) => t;
let groupBy: (string, t) => t;

let limit: (int, t) => t;
let offset: (int, t) => t;