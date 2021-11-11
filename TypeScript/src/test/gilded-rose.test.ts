import { Item, GildedRose } from '../gilded-rose'
import { itemsGoldenMaster } from './golden-master-text-test'

describe('Gilded Rose', function () {
  it('should foo', function () {
    const gildedRose = new GildedRose([new Item('foo', 0, 0)])
    const items = gildedRose.updateQuality()
    expect(items[0].name).toMatchInlineSnapshot(`"foo"`)
    expect(items[0].quality).toMatchInlineSnapshot(`0`)
    expect(items[0].sellIn).toMatchInlineSnapshot(`-1`)
  })

  describe('should keep previous behavior', () => {
    it('for period of 2 days', function () {
      const gildedRose = new GildedRose(itemsGoldenMaster)
      const days = 2
      for (let i = 0; i < days; i++) {
        itemsGoldenMaster.forEach(element => {
          expect(
            `Day ${i + 1} - name: ${element.name} quality: ${element.quality} sellIn: ${
              element.sellIn
            }`,
          ).toMatchSnapshot()
          gildedRose.updateQuality()
        })
      }
    })
    it('for period of 15 days', function () {
      const gildedRose = new GildedRose(itemsGoldenMaster)
      const days = 15
      for (let i = 0; i < days; i++) {
        itemsGoldenMaster.forEach(element => {
          expect(
            `Day ${i + 1} - name: ${element.name} quality: ${element.quality} sellIn: ${
              element.sellIn
            }`,
          ).toMatchSnapshot()
          gildedRose.updateQuality()
        })
      }
    })
  })
})
