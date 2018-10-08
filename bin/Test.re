open Lib.Lego;

let test = (testName, compare, sqlB) => {
  let sql = sqlB |> toSQL;
  if (sql != compare) {
    print_string("Test Failure: " ++ testName ++ "\n");
    print_string("Expected SQL:\n" ++ compare ++ "\n\n");
    print_string("Received SQL:\n" ++ sql ++ "\n\n");
  };
  ();
};

let () = {
  let cats = lego() |> from("cats");
  test("Simple from", {|SELECT * FROM "cats"|}, lego() |> from("cats"));
  test("Simple tableName", {|SELECT * FROM "cats"|}, lego(~tableName="cats", ()));
  test(
    "Select list",
    {|SELECT "1", "2", "3", "4", "5" FROM "cats"|},
    lego() |> selectList(["1", "2"]) |> selectList(["3", "4"]) |> select("5") |> from("cats"),
  );
  test(
    "rawJoin",
    {|SELECT * FROM "cats" JOIN foobar ON foobar.id = cats.foobar_id|},
    cats |> joinRaw("JOIN foobar ON foobar.id = cats.foobar_id"),
  );
  test(
    "multiRawJoin",
    {|SELECT * FROM "cats" JOIN foobar ON foobar.id = cats.foobar_id JOIN barfoo ON barfoo.id = foobar.barfoo_id|},
    lego()
    |> from("cats")
    |> joinRaw("JOIN foobar ON foobar.id = cats.foobar_id")
    |> joinRaw("JOIN barfoo ON barfoo.id = foobar.barfoo_id"),
  );
  test(
    "masterJoin",
    {|SELECT * FROM "cats" JOIN "owners" ON "owners"."id" = "cats"."owner_id" LEFT JOIN "homes" ON "homes"."id" = "owners"."home_id"|},
    cats
    |> join("owners", "owners.id", "cats.owner_id")
    |> leftJoin("homes", "homes.id", "owners.home_id"),
  );
  test(
    "whereNull",
    {|SELECT * FROM "cats" WHERE "name" IS NOT NULL AND "owner_id" IS NULL|},
    cats |> whereNotNull("name") |> whereNull("owner_id"),
  );
  test(
    "whereIn",
    {|SELECT * FROM "cats" WHERE "name" IN ('fluffy', 'socks') AND "owner_id" IN (1, 2, 3) AND "numOfLegs" IN (3.0, 4.0, 3.5)|},
    cats
    |> whereStringIn("name", ["fluffy", "socks"])
    |> whereIntIn("owner_id", [1, 2, 3])
    |> whereFloatIn("numOfLegs", [3., 4., 3.5]),
  );
  test(
    "whereExists",
    {|SELECT * FROM "cats" WHERE EXISTS (SELECT 1 FROM "owners" WHERE owners.id = cats.owner_id)|},
    cats |> whereExists(b => b |> from("owners") |> whereRaw("owners.id = cats.owner_id")),
  );
  test(
    "whereNotExists",
    {|SELECT * FROM "cats" WHERE NOT EXISTS (SELECT 1 FROM "owners" WHERE owners.id = cats.owner_id AND "owners"."name" = 'Bob')|},
    cats
    |> whereNotExists(b =>
         b
         |> from("owners")
         |> whereRaw("owners.id = cats.owner_id")
         |> whereString("owners.name", Equal, "Bob")
       ),
  );
  test(
    "orderRaw",
    {|SELECT * FROM "cats" ORDER BY cats.name ASC NULLS LAST|},
    cats |> orderRaw("cats.name ASC NULLS LAST"),
  );
  test(
    "order",
    {|SELECT * FROM "cats" ORDER BY "cats"."name" ASC, "numOfLogs" DESC|},
    cats |> order("cats.name", ASC) |> order("numOfLogs", DESC),
  );
  test("groupBy", {|SELECT * FROM "cats" GROUP BY "name"|}, cats |> groupBy("name"));
  test("groupByRaw", {|SELECT * FROM "cats" GROUP BY 1|}, cats |> groupByRaw("1"));
  test("limit", {|SELECT * FROM "cats" LIMIT 1|}, cats |> limit(1));
  test("offset", {|SELECT * FROM "cats" OFFSET 1|}, cats |> offset(1));
  test(
    "whereInSub",
    {|SELECT * FROM "cats" WHERE "owner_id" IN (SELECT "id" FROM "owners" WHERE "disabled" IS TRUE)|},
    cats
    |> whereInSub("owner_id", b => b |> select("id") |> from("owners") |> whereTrue("disabled")),
  );
  test(
    "whereInOp",
    {|SELECT * FROM "cats" WHERE "numOfFeet" < (SELECT AVG(numOfFeet) FROM "cats")|},
    cats |> whereSub("numOfFeet", LessThan, b => cats |> selectRaw("AVG(numOfFeet)")),
  );

  let kitchenSink =
    lego()
    |> selectList(["cats.name", "owners.name", "homes.name"])
    |> from("cats")
    |> join("owners", "owners.id", "cats.owner_id")
    |> leftJoin("homes", "homes.id", "owners.home_id")
    |> whereInt("owners.tenant_id", Equal, 55)
    |> whereFloat("cats.numOfLegs", GreaterThanEqual, 4.)
    |> whereString("owners.name", NotEqual, "Bob")
    |> whereNotExists(b => b |> from("families") |> whereRaw("families.id = cats.family_id"))
    |> order("cats.name", ASC)
    |> limit(100)
    |> offset(100);

  test(
    "selectKitchenSink",
    {|SELECT "cats"."name", "owners"."name", "homes"."name" FROM "cats" JOIN "owners" ON "owners"."id" = "cats"."owner_id" LEFT JOIN "homes" ON "homes"."id" = "owners"."home_id" WHERE "owners"."tenant_id" = 55 AND "cats"."numOfLegs" >= 4.0 AND "owners"."name" <> 'Bob' AND NOT EXISTS (SELECT 1 FROM "families" WHERE families.id = cats.family_id) ORDER BY "cats"."name" ASC LIMIT 100 OFFSET 100|},
    kitchenSink,
  );
  test(
    "prettySelectKitchenSink",
    String.trim(
      {|
SELECT "cats"."name",
       "owners"."name",
       "homes"."name"
FROM "cats"
JOIN "owners" ON "owners"."id" = "cats"."owner_id"
LEFT JOIN "homes" ON "homes"."id" = "owners"."home_id"
WHERE "owners"."tenant_id" = 55
AND "cats"."numOfLegs" >= 4.0
AND "owners"."name" <> 'Bob'
AND NOT EXISTS (
  SELECT 1
  FROM "families"
  WHERE families.id = cats.family_id
)
ORDER BY "cats"."name" ASC
LIMIT 100
OFFSET 100
|},
    ),
    {...kitchenSink, prettyPrint: true},
  );
};