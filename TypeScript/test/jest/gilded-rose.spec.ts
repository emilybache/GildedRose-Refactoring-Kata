import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose', () => {
  const gildedRose = new GildedRose([]);
  it('updateStandardItemQuality should update quality and sell in accordingly', () => {
    const oldItem = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
    const updatedItem = gildedRose.updateStandardItemQuality(oldItem);
    expect(updatedItem.quality).toEqual(80);
    expect(updatedItem.sellIn).toEqual(0);
    expect(oldItem.quality).toEqual(updatedItem.quality);

  });

  it('updateStandardItemQuality should update quality and sell in accordingly', () => {
    const oldItem = new Item("Sulfuras, Hand of Ragnaros", -1, 80);
    const updatedItem = gildedRose.updateStandardItemQuality(oldItem);
    expect(updatedItem.quality).toEqual(80);
    expect(updatedItem.sellIn).toEqual(-1);
    expect(oldItem.quality).toEqual(updatedItem.quality);

  });

  it('updatePlus5DexterityVestQuality should update quality and sell in accordingly', () => {
    const oldItem = new Item("+5 Dexterity Vest", 10, 20);
    const updatedItem = gildedRose.updatePlus5DexterityVestQuality(oldItem);
    expect(updatedItem.quality).toEqual(19);
    expect(updatedItem.sellIn).toEqual(9);
    expect(oldItem).not.toEqual(updatedItem);
  });

  it('updateElixirOfTheMongooseQuality should update quality and sell in accordingly', () => {
    const oldItem = new Item("Elixir of the Mongoose", 5, 7);
    const updatedItem = gildedRose.updateElixirOfTheMongooseQuality(oldItem);
    expect(updatedItem.quality).toEqual(6);
    expect(updatedItem.sellIn).toEqual(4);
    expect(oldItem).not.toEqual(updatedItem);
  });

  it('updateBackstagepassesQuality should update quality and sell in accordingly when selling is greater than 10', () => {
    const oldItem =   new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
    const updatedItem = gildedRose.updateBackstagepassesQuality(oldItem);
    expect(updatedItem.quality).toEqual(21);
    expect(updatedItem.sellIn).toEqual(14);
    expect(oldItem).not.toEqual(updatedItem);
  });

  it('updateConjuredManaCakeQuality should update quality and sell in accordingly', () => {
    const oldItem =   new Item("Conjured Mana Cake", 3, 6);
    const updatedItem = gildedRose.updateConjuredManaCakeQuality(oldItem);
    expect(updatedItem.quality).toEqual(4);
    expect(updatedItem.sellIn).toEqual(2);
    expect(oldItem).not.toEqual(updatedItem);
  });

  it('updateBackstagepassesQuality should update quality and sell in accordingly when selling is less than 5', () => {
    const oldItem =   new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49);
    const updatedItem = gildedRose.updateBackstagepassesQuality(oldItem);
    expect(updatedItem.quality).toEqual(50);
    expect(updatedItem.sellIn).toEqual(4);
    expect(oldItem).not.toEqual(updatedItem);
  });

  it('updateBackstagepassesQuality should update quality and sell in accordingly when selling is less than 5', () => {
    const oldItem =   new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49);
    const updatedItem = gildedRose.updateBackstagepassesQuality(oldItem);
    expect(updatedItem.quality).toEqual(50);
    expect(updatedItem.sellIn).toEqual(4);
    expect(oldItem).not.toEqual(updatedItem);
  });

  it('updateAgedBrieQuality should update quality and sell in accordingly', () => {
    const oldItem =   new Item("Aged Brie", 2, 0);
    const updatedItem = gildedRose.updateAgedBrieQuality(oldItem);
    expect(updatedItem.quality).toEqual(1);
    expect(updatedItem.sellIn).toEqual(1);
    expect(oldItem).not.toEqual(updatedItem);
  });
});
