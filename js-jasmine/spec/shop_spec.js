var { Shop } = require('../src/shop.js');

describe('Shop', () => {
  const item = {
    name: 'foo',
    sellIn: 5,
    quality: 5
  };
  const gildedRose = new Shop([item]);
  describe('.updateQuality', () => {
    it("keeps name the same", () => {
      item.name = 'foo';
      items = gildedRose.updateQuality();
      expect(items[0].name).toEqual('foo');
    });
    it('decreases sellIn by 1', () => {
      item.name = 'foo';
      item.sellIn = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toEqual(5 - 1);
    });
    it('decreases qualtiy by 1', () => {
      item.name = 'foo';
      item.sellIn = 5;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(5 - 1);
    });
    it('when sell by passed, decreases quality by 2', () => {
      item.name = 'foo';
      item.sellIn = 0;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(5 - 2);
    });
    it('will not reduce quality below 0', () => {
      item.name = 'foo';
      item.quality = 0;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(0);
    });
    it('will increase the quality of Aged Brie by 1', () => {
      item.name = 'Aged Brie';
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(5 + 1);
    });
    it('will not increase quality above 50', () => {
      item.name = 'Aged Brie';
      item.quality = 50;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(50);
    });
    it('will not change the sellIn or quality of Sulfuras', () => {
      item.name = 'Sulfuras, Hand of Ragnaros';
      item.sellIn = 5;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toEqual(5);
      expect(items[0].quality).toEqual(5);
    });
    it('will increase the quality of backstage passes by 1, more than 10 days before the concert', () => {
      item.name = 'Backstage passes to a TAFKAL80ETC concert';
      item.sellIn = 11;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(6);
    });
    it('will increase the quality of backstage passes by 2, 10 - 6 days before the concert', () => {
      item.name = 'Backstage passes to a TAFKAL80ETC concert';
      item.sellIn = 8;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(7);
    });
    it('will increase the quality of backstage passes by 3, 5 or less days before the concert', () => {
      item.name = 'Backstage passes to a TAFKAL80ETC concert';
      item.sellIn = 5;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(8);
    });
    it('will decrease the quality of backstage passes to 0 after concert.', () => {
      item.name = 'Backstage passes to a TAFKAL80ETC concert';
      item.sellIn = 0;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(0);
    });
  });
});
