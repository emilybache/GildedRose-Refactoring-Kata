import { execSync } from 'node:child_process';
import { Item, GildedRose } from '@/gilded-rose';

/**
 * This test uses Vitest Snapshot, similar to [Jest Snapshot](https://goo.gl/fbAQLP).
 *
 * There are two test cases here with different styles:
 * <li>"foo" is more similar to the unit test from the 'Java' version
 * <li>"thirtyDays" is more similar to the TextTest from the 'Java' version
 *
 * I suggest choosing one style to develop and deleting the other.
 */

describe('Gilded Rose Approval', () => {
  it('should foo', () => {
    const gildedRose = new GildedRose([new Item('foo', 0, 0)]);
    const items = gildedRose.updateQuality();

    expect(items).toMatchSnapshot();
  });

  it('should thirtyDays', () => {
    const consoleOutput = execSync(
      'ts-node test/golden-master-text-test.ts 30',
      { encoding: 'utf-8' }
    );

    expect(consoleOutput).toMatchSnapshot();
  });
});
