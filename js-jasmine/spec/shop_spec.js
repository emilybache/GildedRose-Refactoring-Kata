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
      item.sellIn = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].sellIn).toEqual(5 - 1);
    });
    it('decreases qualtiy by 1', () => {
      item.sellIn = 5;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(5 - 1);
    });
    it('when sell by passed, decreases quality by 2', () => {
      item.sellIn = 0;
      item.quality = 5;
      const items = gildedRose.updateQuality();
      expect(items[0].quality).toEqual(5 - 2);
    });
    it('will not reduce quality below 0', () => {
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
  });
});
