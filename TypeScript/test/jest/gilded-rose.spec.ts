import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose', () => {
  it('test: Aged Brie', () => {
    const gildedRose = new GildedRose([new Item('Aged Brie', 5, 5)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe('Aged Brie');
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(6);
  });
  it('test: Backstage passes to a TAFKAL80ETC concert', () => {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 5, 5)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe('Backstage passes to a TAFKAL80ETC concert');
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(8);
  });
  it('test: Sulfuras, Hand of Ragnaros', () => {
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 5, 80)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe('Sulfuras, Hand of Ragnaros');
    expect(items[0].sellIn).toBe(5);
    expect(items[0].quality).toBe(80);
  });
  it('test: Conjured Mana Cake', () => {
    const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 5, 5)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe('Conjured Mana Cake');
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(3);
  });
  it('test: Elixir of the Mongoose', () => {
    const gildedRose = new GildedRose([new Item('Elixir of the Mongoose', 5, 5)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toBe('Elixir of the Mongoose');
    expect(items[0].sellIn).toBe(4);
    expect(items[0].quality).toBe(4);
  });
});
