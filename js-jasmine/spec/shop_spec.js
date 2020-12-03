var { Shop } = require('../src/shop.js');

describe("Gilded Rose", function () {
  let item = {
    name: 'foo',
    sellIn: 5,
    quality: 5
  }
  it("should foo", function () {
    const gildedRose = new Shop([item]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual("foo");
  });

});
