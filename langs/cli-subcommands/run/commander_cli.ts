// ANCHOR: commander
// TypeScript (commander)
// TypeScript（commander）
import { Command, Option } from "commander";

const program = new Command("mondex")
  .option("-f, --file <file>", "the bestiary file", "monsters.json");

program
  .command("list")
  .addOption(new Option("-e, --element <element>")
    .choices(["Fire", "Water", "Grass"]))
  .option("--json", "print as JSON")
  .action((opts) => { /* ... */ });

program.parse();
// ANCHOR_END: commander
