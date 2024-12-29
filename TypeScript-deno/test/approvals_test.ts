import { beforeEach, describe, it } from "jsr:@std/testing/bdd";
import { GildedRose, Item } from "../app/gilded-rose.ts";
import { assertSnapshot } from "jsr:@std/testing/snapshot";
import { runner } from "./golden-master-text-test.ts";

/**
 * This unit test uses [Jest Snapshot](https://goo.gl/fbAQLP).
 *
 * There are two test cases here with different styles:
 * <li>"foo" is more similar to the unit test from the 'Java' version
 * <li>"thirtyDays" is more similar to the TextTest from the 'Java' version
 *
 * I suggest choosing one style to develop and deleting the other.
 */

describe("Gilded Rose Approval", () => {
  let gameConsoleOutput: string;
  let originalConsoleLog: (message: any) => void;
  let originalProcessArgv: string[];
  let main: (...args: any[]) => void;

  function gameConsoleLog(msg: string) {
    if (msg) {
      gameConsoleOutput += msg;
    }
    gameConsoleOutput += "\n";
  }

  beforeEach(() => {
    // prepare capturing console.log to our own gameConsoleLog.
    gameConsoleOutput = "";
    originalProcessArgv = Deno.args;
    main = runner(gameConsoleLog);
  });

  it("should foo", async (t) => {
    const gildedRose = new GildedRose([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();

    await assertSnapshot(t, items);
  });

  it("should thirtyDays", async (t) => {
    main(["30"]);
    await assertSnapshot(t, gameConsoleOutput);
  });
});
