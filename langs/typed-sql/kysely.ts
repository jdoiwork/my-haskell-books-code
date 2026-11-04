// A typed query builder in TypeScript, for the chapter
// "A small typed SQL".
// TypeScript の型付きクエリビルダー。「小さな型安全 SQL を作る」の章
import {
  Kysely, DummyDriver, SqliteAdapter,
  SqliteIntrospector, SqliteQueryCompiler,
} from "kysely";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: kysely
// TypeScript (Kysely: the table's columns and types are written once)
// TypeScript（Kysely：表の列と型を、1回だけ書く）
interface Database {
  adventurers: { id: number; name: string; job: string; level: number };
}

const db = new Kysely<Database>({ dialect: {
  createAdapter: () => new SqliteAdapter(),
  createDriver: () => new DummyDriver(),
  createIntrospector: (db) => new SqliteIntrospector(db),
  createQueryCompiler: () => new SqliteQueryCompiler(),
} });

const strong = db
  .selectFrom("adventurers")
  .selectAll()
  .where("level", ">=", 3)
  .orderBy("name")
  .compile();

// @ts-expect-error: there is no column "levle"
db.selectFrom("adventurers").selectAll().where("levle", ">=", 3);
// ANCHOR_END: kysely

check(strong.sql === 'select * from "adventurers" where "level" >= ? '
                   + 'order by "name"', strong.sql);
check(strong.parameters[0] === 3, "parameters");
console.log("ok");
