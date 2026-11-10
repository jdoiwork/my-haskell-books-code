// A note as one line.
// 感想を1行に
import { describe, expect, it } from "vitest";
import { noteLine } from "./noteLine";

describe("noteLine", () => {
  // 「見せる感想は本文、ネタバレなら印つき」
  it("shows the body, marked if it is a spoiler", () => {
    const note =
      { entry: null, body: "竜が眠る", writtenAt: "", spoiler: true };
    expect(noteLine({ tag: "ShownNote", note })).toBe("（ネタバレ）竜が眠る");
  });
  // 「隠した感想は、ヒントだけ」
  it("shows only the hint of a hidden note", () => {
    expect(noteLine({ tag: "HiddenNote", entry: 8, hint: "第4章を読んだら" }))
      .toBe("🔒 第4章を読んだら");
  });
});
