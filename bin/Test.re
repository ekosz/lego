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
  test("Simple from", "SELECT * FROM \"cats\";", lego() |> from("cats"));
  test("Simple tableName", "SELECT * FROM \"cats\";", lego(~tableName="cats", ()));
  test(
    "Select list",
    {|SELECT "name", "numOfLegs" FROM "cats";|},
    lego() |> select(["name", "numOfLegs"]) |> from("cats"),
  );
  test(
    "rawJoin",
    "SELECT * FROM \"cats\" JOIN foobar ON foobar.id = cats.foobar_id;",
    lego() |> from("cats") |> joinRaw("JOIN foobar ON foobar.id = cats.foobar_id"),
  );
  test(
    "multiRawJoin",
    {|SELECT * FROM "cats" JOIN foobar ON foobar.id = cats.foobar_id JOIN barfoo ON barfoo.id = foobar.barfoo_id;|},
    lego()
    |> from("cats")
    |> joinRaw("JOIN foobar ON foobar.id = cats.foobar_id")
    |> joinRaw("JOIN barfoo ON barfoo.id = foobar.barfoo_id"),
  );
  test(
    "masterJoin",
    {|SELECT * FROM "cats" JOIN "owners" ON "owners"."id" = "cats"."owner_id" LEFT JOIN "homes" ON "homes"."id" = "owners"."home_id";|},
    lego()
    |> from("cats")
    |> join("owners", "owners.id", "cats.owner_id")
    |> leftJoin("homes", "homes.id", "owners.home_id"),
  );
  test(
    "selectKitchenSink",
    {|SELECT "cats"."name", "owners"."name", "homes"."name" FROM "cats" JOIN "owners" ON "owners"."id" = "cats"."owner_id" LEFT JOIN "homes" ON "homes"."id" = "owners"."home_id" WHERE "owners"."tenant_id" = 55 AND "cats"."numOfLegs" >= 4.0 AND "owners"."name" <> 'Bob';|},
    lego()
    |> select(["cats.name", "owners.name", "homes.name"])
    |> from("cats")
    |> join("owners", "owners.id", "cats.owner_id")
    |> leftJoin("homes", "homes.id", "owners.home_id")
    |> whereInt("owners.tenant_id", Equal, 55)
    |> whereFloat("cats.numOfLegs", GreaterThanEqual, 4.)
    |> whereString("owners.name", NotEqual, "Bob"),
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
AND "owners"."name" <> 'Bob';
|},
    ),
    lego(~prettyPrint=true, ())
    |> select(["cats.name", "owners.name", "homes.name"])
    |> from("cats")
    |> join("owners", "owners.id", "cats.owner_id")
    |> leftJoin("homes", "homes.id", "owners.home_id")
    |> whereInt("owners.tenant_id", Equal, 55)
    |> whereFloat("cats.numOfLegs", GreaterThanEqual, 4.)
    |> whereString("owners.name", NotEqual, "Bob"),
  );
};