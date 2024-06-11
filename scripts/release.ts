import { writeFile } from "fs/promises";
import { GHRelEasy } from "gh-rel-easy";
import { exec } from "child_process";
import { promisify } from "node:util";
import { Command } from "commander";

export const exit = (error: Error) => {
  console.error(error.message);
  process.exit(1);
};

const BUFFER = 10 * 1024 * 1024;

// HCONF
const hconf = async (cmd: "version" | "next" | "setup", ...ops: string[]) => {
  const { stdout } = await promisify(exec)(
    ["hconf", cmd, ops].flat().join(" "),
    { maxBuffer: BUFFER, encoding: "utf-8" }
  );

  if (cmd !== "version") {
    console.log(stdout);
  }

  return stdout.trim();
};

// GHRelEasy
const release = new GHRelEasy({
  gh: "morpheusgraphql/morpheus-graphql",
  scope: {
    server: "morpheus-graphql",
    client: "morpheus-graphql-client",
    core: "morpheus-graphql-core",
    subscriptions: "morpheus-graphql-subscriptions",
    tests: "morpheus-graphql-tests",
    app: "morpheus-graphql-app",
  },
  version: () => hconf("version"),
  pkg: (p) => `https://hackage.haskell.org/package/${p}`,
  next: (b) => hconf("next", ...(b ? ["-b"] : [])).then(() => hconf("version")),
});

// CLI
const cli = new Command();

cli.name("release-cli").description("Automated Releases").version("1.0");

cli
  .command("open")
  .option("-p, --preview", "preview", false)
  .action(({ preview }: { preview: boolean }) =>
    release
      .changelog()
      .then((body) =>
        hconf("setup").then(() => (preview ? undefined : release.open(body)))
      )
      .catch(exit)
  );

cli.command("changelog").action(() =>
  release
    .changelog()
    .then((body: string) => writeFile("./changelog.md", body, "utf8"))
    .catch(exit)
);

cli.parse();
