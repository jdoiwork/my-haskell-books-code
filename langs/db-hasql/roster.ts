// The Kysely code in the hasql and rel8 chapter.
// It only builds SQL (no database), so it needs no PostgreSQL.
// hasql と rel8 の章の Kysely のコード。
// SQL を組み立てるだけ（データベースにはつながない）
import {
  Kysely, DummyDriver, PostgresAdapter,
  PostgresIntrospector, PostgresQueryCompiler,
} from "kysely";
import type { Generated } from "kysely";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: kysely
// TypeScript (a typed query builder: Kysely)
// TypeScript（型付きのクエリビルダー：Kysely）
interface AdventurerTable {
  id: Generated<number>;
  name: string;
  job: string;
  level: number;
}

interface Database {
  adventurers: AdventurerTable;
}

const db = new Kysely<Database>({ dialect: {
  createAdapter: () => new PostgresAdapter(),
  createDriver: () => new DummyDriver(),
  createIntrospector: (db) => new PostgresIntrospector(db),
  createQueryCompiler: () => new PostgresQueryCompiler(),
} });

function strong(lv: number) {
  return db
    .selectFrom("adventurers")
    .select(["name", "level"])
    .where("level", ">=", lv)
    .orderBy("level", "desc");
}

// @ts-expect-error: there is no column "levle".
db.selectFrom("adventurers").select(["name", "levle"]);
// ANCHOR_END: kysely

const compiled = strong(3).compile();
check(
  compiled.sql ===
    'select "name", "level" from "adventurers" ' +
      'where "level" >= $1 order by "level" desc',
  "sql",
);
check(JSON.stringify(compiled.parameters) === "[3]", "parameters");
console.log("ok");
