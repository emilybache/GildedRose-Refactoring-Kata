var { Shop } = require('../src/shop.js');

describe('Shop', () => {
  const item = {
    name: 'foo',
    sellIn: 5,
    quality: 5
  };
  const gildedRose = new Shop([item]);
  let items = []
  describe('.updateQuality', () => {
    it("keeps name the same", () => {
      items = gildedRose.updateQuality();
      expect(items[0].name).toEqual("foo");
    });
    describe('when sellIn and quality are above 0', () => {
      beforeEach(() => {
        item.sellIn = 5;
        item.quality = 5;
        items = gildedRose.updateQuality();
      });
      it('descreases sellIn by 1', () => {
        expect(items[0].sellIn).toEqual(5 - 1);
      });
      it('decreases quality by 1', () => {
        expect(items[0].sellIn).toEqual(5 - 1);
      });
    });
    describe('when the sell by date has passed', () => {
      beforeEach(() => {
        item.sellIn = 0;
        item.quality = 5;
        items = gildedRose.updateQuality();
      });
      it('quality degrades twice as fast', () => {
        expect(items[0].quality).toEqual(5 - 2)
      })
    })
  });
});
