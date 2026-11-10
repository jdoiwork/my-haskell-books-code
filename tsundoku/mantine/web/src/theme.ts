// ANCHOR: all
// 🎨 The colours of the shelf, as a Mantine theme. What theme.css said
//    with CSS variables (--accent, --done, ...), the theme says here; the
//    shapes (shelf.css) are the components' own now. Dark mode comes
//    with Mantine: each colour has ten shades, light and dark.
// 本棚の色を、Mantine のテーマに。theme.css が CSS の変数（--accent、
// --done など）で言っていたことを、ここで言う。形（shelf.css）は、もう
// 部品が持っている。ダークモードは Mantine に付いてくる：どの色も、明るい
// 方から暗い方まで10段ある
import { createTheme } from "@mantine/core";

const font = '-apple-system, "Hiragino Sans", "Noto Sans JP", sans-serif';

export const theme = createTheme({
  primaryColor: "indigo",          // --accent: the band, buttons, bars
  defaultRadius: "md",
  fontFamily: font,
  headings: { fontFamily: font },
  // The colours with a meaning on this shelf, by name.
  // この本棚で意味のある色を、名前で
  other: {
    done:   "teal",                // --done: read / 読んだ
    star:   "yellow",              // --star: the stars / ★
    mark:   "yellow",              // --mark: a word found / 見つけた語
    warn:   "yellow",              // --warn: before losing / 消える前
    danger: "red",                 // --danger: refused / 断られた
  },
});

// 🏷️ The same names, for the components (theme.other is untyped).
// 同じ名前を、部品のために（theme.other には型がない）
export const tone = theme.other as {
  done: string; star: string; mark: string; warn: string; danger: string;
};
// ANCHOR_END: all
