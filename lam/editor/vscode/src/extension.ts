// ANCHOR: all
// 🧩 The VS Code extension of Lam: the colors come from the grammar
//    (package.json says where it is); this file starts lam lsp when a
//    .lam file is opened, and lets the language client talk to it.
// Lam の VS Code の拡張。色は文法から（場所は package.json に書く）。
// このファイルは、.lam のファイルが開かれたら lam lsp を起動し、言語
// クライアントにサーバーと話してもらう
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

// ANCHOR: activate
// ▶️ VS Code calls this once, when the first .lam file is opened.
// 最初の .lam のファイルが開かれたときに、VS Code が1度だけ呼ぶ
export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  // The lam to start: the setting lam.path, or the lam on the PATH.
  // 起動する lam：設定の lam.path か、PATH の上の lam
  const lam = vscode.workspace
    .getConfiguration("lam")
    .get<string>("path", "lam");
  // lam lsp talks on its standard input and output.
  // lam lsp は、標準入力と標準出力で話す
  const server: ServerOptions = { command: lam, args: ["lsp"] };
  const options: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "lam" }],
  };
  client = new LanguageClient("lam", "Lam", server, options);
  context.subscriptions.push(client);
  await client.start();
}
// ANCHOR_END: activate

// ⏹️ VS Code calls this when it closes: say shutdown and exit to lam.
// VS Code が閉じるときに呼ぶ。lam に shutdown と exit を言う
export async function deactivate(): Promise<void> {
  await client?.stop();
}
// ANCHOR_END: all
