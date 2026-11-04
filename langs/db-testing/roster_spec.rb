# The RSpec code in the database testing chapter.
# データベースをテストする章の RSpec のコード
require "sqlite3"

def create_table(db)
  db.execute(<<~SQL)
    CREATE TABLE adventurers (id INTEGER PRIMARY KEY,
      name TEXT NOT NULL UNIQUE, job TEXT NOT NULL,
      level INTEGER NOT NULL)
  SQL
end

def add_adventurer(db, name, job, level)
  db.execute(
    "INSERT INTO adventurers (name, job, level) VALUES (?, ?, ?)",
    [name, job, level])
end

def names(db)
  db.execute("SELECT name FROM adventurers ORDER BY id").map(&:first)
end

# ANCHOR: around
# Ruby (RSpec: around wraps each example)
# Ruby（RSpec：around で、テストを1つずつ包む）
RSpec.describe "the roster" do
  around do |example|
    @db = SQLite3::Database.new(":memory:")
    create_table(@db)
    example.run
    @db.close
  end

  # 「空っぽから始まる」
  it "starts empty" do
    expect(names(@db)).to eq([])
  end

  # 「同じ名前は、2回は登録できない」
  it "rejects the same name twice" do
    add_adventurer(@db, "Lambda", "Mage", 3)
    expect { add_adventurer(@db, "Lambda", "Mage", 3) }
      .to raise_error(SQLite3::ConstraintException)
  end
end
# ANCHOR_END: around
