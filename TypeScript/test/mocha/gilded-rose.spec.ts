import { expect } from 'chai';
import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose', () => {
  it('should foo', () => {
    const gildedRose = new GildedRose([new Item('foo', 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).to.equal('foo');
  });
  it('should degrade', () => {
    const gildedRose = new GildedRose([new Item('Cheese', 5, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(19);
  });
  it('should degrade twice as fast past the expiration date', () => {
    const gildedRose = new GildedRose([new Item('Cheese', 0, 20)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(18);
  });
  it('should not have negative quality', () => {
    const gildedRose = new GildedRose([new Item('test', 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(0);
  });
  it('should increase quality for aged brie', () => {
    const gildedRose = new GildedRose([new Item('Aged Brie', 2, 25)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(26);
  });
  it('should not increase quality past 50', () => {
    const gildedRose = new GildedRose([new Item('Aged Brie', 0, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(50);
  });
  it('should not decrease quality or sellIn value for Sulfuras', () => {
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 30, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(50);
    expect(items[0].sellIn).to.equal(30);
  });
  it('should increase quality for backstage passes', () => {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 20, 30)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(31);
  });
  it('should increase quality for backstage passes by 2 when 10 days are left', () => {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 10, 30)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(32);
  });
  it('should increase quality for backstage passes by 3 when 5 days are left', () => {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 5, 40)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(43);
  });
  it('should set quality for backstage passes to 0 after the date', () => {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 0, 30)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(0);
  });
  it('should increase quality by 2 for aged brie past sellin', () => {
    const gildedRose = new GildedRose([new Item('Aged Brie', 0, 30)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(32);
  });
  it('should decrease Conjured quality by 2', () => {
    const gildedRose = new GildedRose([new Item('Conjured', 10, 30)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(28);
  });
  it('should decrease Conjured quality by 4 past sellin', () => {
    const gildedRose = new GildedRose([new Item('Conjured', 0, 30)]);
    const items = gildedRose.updateQuality();
    expect(items[0].quality).to.equal(26);
  });
});
