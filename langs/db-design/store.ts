// The TypeScript code in the DB-independent design chapter.
// DB に縛られない設計の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: interface
// TypeScript (an interface, and a class for each store)
// TypeScript（インターフェースと、置き場ごとのクラス）
interface Adventurer { id: number; name: string; job: string; level: number }

type AddResult = { ok: true; id: number } | { ok: false; taken: string };

interface RosterStore {
  addAdventurer(name: string, job: string, level: number): AddResult;
  listAdventurers(): Adventurer[];
}

class MemoryStore implements RosterStore {
  private byName = new Map<string, Adventurer>();
  private nextId = 1;

  addAdventurer(name: string, job: string, level: number): AddResult {
    if (this.byName.has(name)) return { ok: false, taken: name };
    const id = this.nextId++;
    this.byName.set(name, { id, name, job, level });
    return { ok: true, id };
  }

  listAdventurers(): Adventurer[] {
    return [...this.byName.values()].sort((a, b) => a.id - b.id);
  }
}

// The guild's work only knows the interface.
// ギルドの仕事は、インターフェースだけを知っている
function enlist(store: RosterStore, name: string, job: string,
                level: number): string {
  const result = store.addAdventurer(name, job, level);
  return result.ok ? `Welcome, ${name}! (no. ${result.id})`
                   : `The name ${result.taken} is taken.`;
}
// ANCHOR_END: interface

const store = new MemoryStore();
const first = enlist(store, "Lambda", "Mage", 3);
check(first === "Welcome, Lambda! (no. 1)", "1");
const second = enlist(store, "Lambda", "Mage", 3);
check(second === "The name Lambda is taken.", "2");
check(store.listAdventurers().length === 1, "list");
