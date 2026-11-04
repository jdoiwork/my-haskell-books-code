// ANCHOR: commander
// TypeScript (commander)
// TypeScript（commander）
import { Command, Option } from "commander";

const program = new Command("monlist")
  .option("-f, --file <file>", "the bestiary file", "monsters.json")
  .addOption(new Option("-e, --element <element>")
    .choices(["Fire", "Water", "Grass"]))
  .option("--sort <key>", "sort by name or hp", "name")
  .option("--json", "print as JSON");

program.parse();
const opts = program.opts();
// ANCHOR_END: commander
console.log(JSON.stringify(opts));
