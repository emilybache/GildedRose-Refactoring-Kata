var { Shop } = require('../src/shop.js');

describe('Shop', () => {
  const item = {
    name: 'foo',
    sellIn: 5,
    quality: 5
  };
  const gildedRose = new Shop([item]);
  describe('.updateQuality', () => {
    describe('Standard item', () => {
      beforeEach(() => {
        item.name = 'foo';
      });
      it("keeps name the same", () => {
        const items = gildedRose.updateQuality();
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
    });

    describe('Aged Brie', () => {
      beforeEach(() => {
        item.name = 'Aged Brie';
      });
      it('will increase the quality by 1', () => {
        item.sellIn = 5;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(5 + 1);
      });
      it('will not increase quality above 50', () => {
        item.quality = 50;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(50);
      });
    });

    describe('Sulfuras', () => {
      beforeEach(() => {
        item.name = 'Sulfuras, Hand of Ragnaros';
      });
      it('will not change the sellIn or quality', () => {
        item.sellIn = 5;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].sellIn).toEqual(5);
        expect(items[0].quality).toEqual(5);
      });
    });

    describe('Backstage passes', () => {
      beforeEach(() => {
        item.name = 'Backstage passes to a TAFKAL80ETC concert';
      });
      it('will increase the quality by 1, more than 10 days before the concert', () => {
        item.sellIn = 11;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(6);
      });
      it('will increase the quality by 2, 10 - 6 days before the concert', () => {
        item.sellIn = 8;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(7);
      });
      it('will increase the quality by 3, 5 or less days before the concert', () => {
        item.sellIn = 5;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(8);
      });
      it('will decrease the quality to 0 after concert.', () => {
        item.sellIn = 0;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(0);
      });
    });

    describe('Conjured', () => {
      beforeEach(() => {
        item.name = 'Conjured pickle';
      });
      it('will decrease the quality by 2 before sell by', () => {
        item.sellIn = 5;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(5 - 2);
      });
      it('will decrease the quality by 4 after sell by', () => {
        item.sellIn = 0;
        item.quality = 5;
        const items = gildedRose.updateQuality();
        expect(items[0].quality).toEqual(5 - 4);
      });
    });
  });
});
