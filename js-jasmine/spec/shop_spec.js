var { Shop } = require('../src/shop.js');

describe(Shop, function () {
  let item = {
    name: 'foo',
    sellIn: 5,
    quality: 5
  }
  describe('.updateQuality', function () {
    it("keeps name the same", function () {
      const gildedRose = new Shop([item]);
      const items = gildedRose.updateQuality();
      expect(items[0].name).toEqual("foo");
    });
  });
});
