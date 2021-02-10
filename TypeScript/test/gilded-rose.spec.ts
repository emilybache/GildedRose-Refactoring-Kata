import { expect } from 'chai'
import { Item, GildedRose } from '../app/gilded-rose'

describe('Gilded Rose', function () {
  it('should foo', function () {
    const gildedRose = new GildedRose([new Item('foo', 0, 0)])
    const items = gildedRose.updateQuality()
    expect(items[0].name).to.equal('fixme')
  })

  it('should not change quality on legendary items', function () {
    const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 12, 80)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(80)
  })

  it('should set the quality to 0 when the Sellin day is 0 for tickets', function () {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 0, 12)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(0)
  })

  it('should increase the quality by 2 when the Sellin day is below 10 for tickets', function () {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 11, 10)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(12)
  })

  it('should increase the quality by 3 when the Sellin day is below 5 for tickets', function () {
    const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 6, 10)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(13)
  })

  it('should increase the quality by 1 when the Sellin day is below 0 for Brie', function () {
    const gildedRose = new GildedRose([new Item('Aged Brie', -1, 4)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(5)
  })

  it('should decrease the quality by 1 when the Sellin day is above 0 for Brie', function () {
    const gildedRose = new GildedRose([new Item('Aged Brie', 5, 4)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(3)
  })

  it('should decrease the quality by 4 when the Sellin day is below 0 for Conjured', function () {
    const gildedRose = new GildedRose([new Item('Conjured', 0, 8)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(4)
  })

  it('should decrease the quality by 2 when the Sellin day is above 0 for Conjured', function () {
    const gildedRose = new GildedRose([new Item('Conjured', 3, 8)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(6)
  })

  it('should not decrease the quality when it is already 0', function () {
    const gildedRose = new GildedRose([new Item('Conjured', 5, 0)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(0)
  })

  it('should not increase the quality when it is already 50', function () {
    const gildedRose = new GildedRose([new Item('Aged Brie', 0, 50)])
    const items = gildedRose.updateQuality()
    expect(items[0].quality).to.equal(50)
  })
})
