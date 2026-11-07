// Scouts sent out at the same time, for the chapter
// "Structured concurrency". StructuredTaskScope is a preview feature,
// so run it with `java --enable-preview --source 26 Scouts.java`.
// 同時に送り出す偵察隊。「構造化された並行性」の章。
// StructuredTaskScope はプレビューの機能なので、
// `java --enable-preview --source 26 Scouts.java` で動かす
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.StructuredTaskScope;

final List<String> report = new CopyOnWriteArrayList<>();

String scout(String name, long millis) throws InterruptedException {
    Thread.sleep(millis);
    report.add(name);
    return name;
}

String trap(long millis) throws InterruptedException {
    Thread.sleep(millis);
    throw new IllegalStateException("fell into a trap");
}

List<String> party() throws InterruptedException {
// ANCHOR: scope
// Java (StructuredTaskScope, still a preview feature)
// Java（StructuredTaskScope。まだプレビューの機能）
try (var scope = StructuredTaskScope.open()) {
    var slime  = scope.fork(() -> scout("slime", 50));
    var trapped = scope.fork(() -> trap(10));
    var goblin = scope.fork(() -> scout("goblin", 100));
    scope.join();
    return List.of(slime.get(), trapped.get(), goblin.get());
}
// ANCHOR_END: scope
}

void main() throws InterruptedException {
    try {
        party();
        throw new AssertionError("should have failed");
    } catch (StructuredTaskScope.FailedException e) {
        if (!(e.getCause() instanceof IllegalStateException)) throw e;
    }
    // join() cancelled the others when the trap went off.
    // わながはじけたとき、join() がほかの仲間をキャンセルした
    Thread.sleep(200);
    if (!report.isEmpty()) throw new AssertionError(report.toString());
    IO.println("ok");
}
