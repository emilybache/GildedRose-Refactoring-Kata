var {expect} = require('chai');
var {Shop, Item} = require('../src/gilded_rose.js');

function getItems(items = []) {
  const gildedRose = new Shop(items)
  return gildedRose.updateQuality();
}

describe("Gilded Rose", function() {
  function assertFirstItemEquals(items, item) {
    expect(items[0].name).to.equal(item.name)
    expect(items[0].sellIn).to.equal(item.sellIn)
    expect(items[0].quality).to.equal(item.quality)
  }
  it("should foo", function() {
    const items = getItems([ new Item("foo", 0, 0) ]);
    expect(items[0].name).to.equal("foo");
  });
  it("should be empty", function () {
    const items = getItems();
    expect(items).to.be.an('array').that.is.empty;
  });
  it("should accept aged Aged Brie", () => {
    const items = getItems([ new Item("Aged Brie", 0, 0) ])
    assertFirstItemEquals(items, {name: 'Aged Brie', sellIn: -1, quality: 2});
  })
  it("Sulfuras, Hand of Ragnaros", () => {
    const items = getItems([new Item("Sulfuras, Hand of Ragnaros", 0, 0) ])
    assertFirstItemEquals(items, {name: 'Sulfuras, Hand of Ragnaros', sellIn: 0, quality: 0});
  })
  it('should accept backstage passes to TAFAL80ETC', ()=>{
    const items = getItems([new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)]);
    assertFirstItemEquals(items, {name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 4, quality: 50});
  })
  it('should subtract quality from itself when sell in is 0 and name is TAFAL80ETC', ()=>{
    const items = getItems([new Item("Backstage passes to a TAFKAL80ETC concert", 0, 50)]);
    assertFirstItemEquals(items, { name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: -1, quality: 0});
  })
  it('should increase quality by 1 when backstage passes to TAFAL80ETC', ()=>{
    const items = getItems([new Item("Backstage passes to a TAFKAL80ETC concert", 12, 49)]);
    assertFirstItemEquals(items, { name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 11, quality: 50});
  })
  it('should decrease quality by 2 when name unheard of', ()=>{
    const items = getItems([new Item("Backstage passes to a TAFKAL80ETC concert", -4, 49)]);
    assertFirstItemEquals(items, {name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: -5, quality: 0});
  })
  it('should decrease quality by 2 when name unheard of', ()=>{
    const items = getItems([new Item("unheard of", -4, 49)]);
    assertFirstItemEquals(items, {name: 'unheard of', sellIn: -5, quality: 47});
  })
});
