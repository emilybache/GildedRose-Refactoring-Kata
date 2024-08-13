import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose', () => {
  it('should foo', () => {
    const gildedRose = new GildedRose([new Item('foo', 0, 0)]);
    const startTime = performance.now();
    const items = gildedRose.updateQuality();
    const endTime = performance.now();

    console.log('Execution time: ' + (endTime - startTime) + 'ms');
    expect(items[0].name).toBe('foo');
  });
});
