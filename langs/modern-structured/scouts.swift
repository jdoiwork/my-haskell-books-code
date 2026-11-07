// Scouts sent out at the same time, for the chapter
// "Structured concurrency".
// 同時に送り出す偵察隊。「構造化された並行性」の章
import Foundation

actor Report {
    var names: [String] = []
    func add(_ name: String) { names.append(name) }
}

struct Trapped: Error {}

let report = Report()

func scout(_ name: String, _ millis: UInt64) async throws -> String {
    try await Task.sleep(nanoseconds: millis * 1_000_000)
    await report.add(name)
    return name
}

func trap(_ millis: UInt64) async throws -> String {
    try await Task.sleep(nanoseconds: millis * 1_000_000)
    throw Trapped()
}

// ANCHOR: group
// Swift (a task group ends when every child task ends)
// Swift（タスクグループは、子どものタスクが全部終わると終わる）
func party() async throws -> [String] {
    try await withThrowingTaskGroup(of: String.self) { group in
        group.addTask { try await scout("slime", 50) }
        group.addTask { try await trap(10) }
        group.addTask { try await scout("goblin", 100) }
        return try await group.reduce(into: []) { $0.append($1) }
    }
}
// ANCHOR_END: group

do {
    _ = try await party()
    preconditionFailure("should have failed")
} catch is Trapped {
}
// The rest were cancelled when the error left the group.
// 失敗がグループの外へ出るとき、残りはキャンセルされた
try await Task.sleep(nanoseconds: 200_000_000)
let names = await report.names
precondition(names.isEmpty)
print("ok")
