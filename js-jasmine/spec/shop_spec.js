var { Shop } = require('../src/shop.js');

describe(Shop, () => {
  const item = {
    name: 'foo',
    sellIn: 5,
    quality: 5
  }
  beforeEach(() => {
    item.name = 'foo';
    item.sellIn = 5;
    item.quality = 5;
  })
  const gildedRose = new Shop([item]);
  describe('.updateQuality', () => {
    it("keeps name the same", () => {
      const items = gildedRose.updateQuality();
      expect(items[0].name).toEqual("foo");
    });
    describe('when sellIn and quality are above 0', () => {
      it('descreases sellIn by 1', () => {
        const items = gildedRose.updateQuality();
        expect(items[0].sellIn).toEqual(4);
      });
    });
  });
});
