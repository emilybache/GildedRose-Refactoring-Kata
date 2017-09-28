describe('Gilded Rose', function() {

  describe('with default item', function() {
    const item = new Item('foo', 0, 0)
    const items = [item]
    const gildedRose = new Shop([item])

    it('has foo item', function() {
      expect(items[0].name).toEqual('foo')
    })

    it('has foo item with 0 sellIn', function() {
      expect(items[0].sellIn).toEqual(0)
    })

    it('has foo item with 0 quality', function() {
      expect(items[0].quality).toEqual(0)
    })
  })

  describe('#downgradeQuality', function() {
    const item = new Item('foo', 10, 20)
    const items = [item]
    const gildedRose = new Shop([item])

    it('downgrade quality by 1', function() {
      gildedRose.downgradeQuality(items[0])

      expect(items[0].quality).toEqual(19)
    })

    it('downgrade quality by 5', function() {
      gildedRose.downgradeQuality(items[0], 5)

      expect(items[0].quality).toEqual(14)
    })
  })

  describe('#upgradeQuality', function() {
    const item = new Item('foo', 10, 20)
    const items = [item]
    const gildedRose = new Shop([item])

    it('upgrade quality by 1', function() {
      gildedRose.upgradeQuality(items[0])

      expect(items[0].quality).toEqual(21)
    })

    it('upgrade quality by 5', function() {
      gildedRose.upgradeQuality(items[0], 5)

      expect(items[0].quality).toEqual(26)
    })
  })

  describe('#downgradeSellIn', function() {
    const item = new Item('foo', 10, 20)
    const items = [item]
    const gildedRose = new Shop([item])

    it('downgrade quality by 1', function() {
      gildedRose.downgradeSellIn(items[0])

      expect(items[0].sellIn).toEqual(9)
    })

    it('downgrade quality by 5', function() {
      gildedRose.downgradeSellIn(items[0], 5)

      expect(items[0].sellIn).toEqual(4)
    })
  })

  describe('#hasQuality', function() {
    const item = new Item('foo', 10, 20)
    const items = [item]
    const gildedRose = new Shop([item])

    it('has quality', function() {
      const hasQuality = gildedRose.hasQuality(items[0])

      expect(hasQuality).toEqual(true)
    })
  })

  describe('#commonQuality', function() {
    const item = new Item('foo', 10, 20)
    const items = [item]
    const gildedRose = new Shop([item])

    it('is a common quality', function() {
      const commonQuality = gildedRose.commonQuality(items[0])

      expect(commonQuality).toEqual(true)
    })
  })

  describe('after updateQuality', function() {
    const item = new Item('foo', 10, 20)
    const items = [item]
    const gildedRose = new Shop([item])
    gildedRose.updateQuality()

    it('has a good quality value', function() {
      expect(items[0].quality).toEqual(19)
    })

    it('has a good sell in value', function() {
      expect(items[0].sellIn).toEqual(9)
    })

    // TODO: Add custom tests with custom names, etc.
  })
})
