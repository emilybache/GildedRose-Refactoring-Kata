import { Item, GildedRose } from '../gilded-rose'

describe('Gilded Rose: ', function () {
  describe('legendary product', () => {
    it('should always have 80 in quality', function () {
      const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 0, 80)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('Sulfuras, Hand of Ragnaros')
      expect(items[0].quality).toBe(80)
      expect(items[0].sellIn).toBe(-1)
      gildedRose.updateQuality()
      expect(items[0].quality).toBe(80)
      expect(items[0].sellIn).toBe(-2)
    })
  })

  describe('Aged Brie product', () => {
    it('should grow in quality', function () {
      const gildedRose = new GildedRose([new Item('Aged Brie', 2, 10)])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.name).toBe('Aged Brie')
      expect(product.quality).toBe(11)
      expect(product.sellIn).toBe(1)
    })

    it('quality should never go above 50', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', 1, 50)])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.name).toBe('Aged Brie')
      expect(product.quality).toBe(50)
      expect(product.sellIn).toBe(0)
    })

    it('should allow quality of outdated aged brie to be incremented up', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', -10, 10)])
      const items = gildedRose.updateQuality()
      const added = items[0]
      expect(added.quality).toBe(11)
      expect(added.sellIn).toBe(-11)
    })
  })

  describe('Backstage product', () => {
    it('should have growing in quality', function () {
      const gildedRose = new GildedRose([
        new Item('Backstage passes to a TAFKAL80ETC concert', 20, 20),
      ])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.name).toBe('Backstage passes to a TAFKAL80ETC concert')
      expect(product.quality).toBe(21)
      expect(product.sellIn).toBe(19)
    })

    it('quality should never go above 50', () => {
      const gildedRose = new GildedRose([
        new Item('Backstage passes to a TAFKAL80ETC concert', 30, 50),
      ])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.name).toBe('Backstage passes to a TAFKAL80ETC concert')
      expect(product.quality).toBe(50)
      expect(product.sellIn).toBe(29)
    })

    it('quality should grow by 2 when sellIn is lower than 11', () => {
      const gildedRose = new GildedRose([
        new Item('Backstage passes to a TAFKAL80ETC concert', 10, 10),
      ])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.quality).toBe(12)
      expect(product.sellIn).toBe(9)
    })

    it('quality should grow by 3 when sellIn is lower than 6', () => {
      const gildedRose = new GildedRose([
        new Item('Backstage passes to a TAFKAL80ETC concert', 5, 10),
      ])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.quality).toBe(13)
      expect(product.sellIn).toBe(4)
    })

    it('quality should be 0 when sellIn is 0 or lower', () => {
      const gildedRose = new GildedRose([
        new Item('Backstage passes to a TAFKAL80ETC concert', 1, 10),
      ])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.quality).toBe(0)
      expect(product.sellIn).toBe(0)
    })

    it('should not decrement below 0 in quality', () => {
      const gildedRose = new GildedRose([
        new Item('Backstage passes to a TAFKAL80ETC concert', 0, 10),
      ])
      const items = gildedRose.updateQuality()
      const product = items[0]
      expect(product.quality).toBe(0)
      expect(product.sellIn).toBe(-1)
    })
  })
  describe('Normal product', () => {
    it('should decrement in quality each day', function () {
      const gildedRose = new GildedRose([new Item('foo', 10, 10)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('foo')
      expect(items[0].quality).toBe(9)
      expect(items[0].sellIn).toBe(9)
      gildedRose.updateQuality()
      expect(items[0].quality).toBe(8)
      expect(items[0].sellIn).toBe(8)
    })
    it('should decrement x2 faster in quality each day when sellIn < 0', function () {
      const gildedRose = new GildedRose([new Item('foo', 0, 10)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('foo')
      expect(items[0].quality).toBe(8)
      expect(items[0].sellIn).toBe(-1)
      gildedRose.updateQuality()
      expect(items[0].quality).toBe(6)
      expect(items[0].sellIn).toBe(-2)
    })
    it('should not decrement below 0 in quality', function () {
      const gildedRose = new GildedRose([new Item('foo', 0, 0)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('foo')
      expect(items[0].quality).toBe(0)
      expect(items[0].sellIn).toBe(-1)
    })
  })
  describe('Conjured product', () => {
    it('should decrement 2 times faster than product in quality each day', function () {
      const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 1, 6)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('Conjured Mana Cake')
      expect(items[0].quality).toBe(4)
      expect(items[0].sellIn).toBe(0)
    })
    it('should decrement x4 faster in quality each day when sellIn < 0', function () {
      const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 0, 6)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('Conjured Mana Cake')
      expect(items[0].quality).toBe(2)
      expect(items[0].sellIn).toBe(-1)
    })
    it('should not decrement below 0 in quality', function () {
      const gildedRose = new GildedRose([new Item('Conjured Mana Cake', 0, 0)])
      const items = gildedRose.updateQuality()
      expect(items[0].name).toBe('Conjured Mana Cake')
      expect(items[0].quality).toBe(0)
      expect(items[0].sellIn).toBe(-1)
    })
  })
})
