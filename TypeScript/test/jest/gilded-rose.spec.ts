import { Item, GildedRose } from '@/gilded-rose'

/**
 * A copy of the unrefactored GildedRose class used for acceptance tests
 */
export class AcceptanceGildedRose {
  items: Array<Item>

  constructor(items = [] as Array<Item>) {
    this.items = items
  }

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
        if (this.items[i].quality > 0) {
          if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
            this.items[i].quality = this.items[i].quality - 1
          }
        }
      } else {
        if (this.items[i].quality < 50) {
          this.items[i].quality = this.items[i].quality + 1
          if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].sellIn < 11) {
              if (this.items[i].quality < 50) {
                this.items[i].quality = this.items[i].quality + 1
              }
            }
            if (this.items[i].sellIn < 6) {
              if (this.items[i].quality < 50) {
                this.items[i].quality = this.items[i].quality + 1
              }
            }
          }
        }
      }
      if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
        this.items[i].sellIn = this.items[i].sellIn - 1
      }
      if (this.items[i].sellIn < 0) {
        if (this.items[i].name != 'Aged Brie') {
          if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].quality > 0) {
              if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
                this.items[i].quality = this.items[i].quality - 1
              }
            }
          } else {
            this.items[i].quality = this.items[i].quality - this.items[i].quality
          }
        } else {
          if (this.items[i].quality < 50) {
            this.items[i].quality = this.items[i].quality + 1
          }
        }
      }
    }

    return this.items
  }
}

const names = ['Aged Brie', 'Backstage passes to a TAFKAL80ETC concert', 'Sulfuras, Hand of Ragnaros', 'Other']
const sellIns = [-1, 0, 1, 2, 5, 6, 7, 10, 11, 12, 13, 14, 15]
const qualities = [-1, 1, 2, 48, 49, 50, 51]

test('acceptance tests', () => {

  for (const name of names) {
    for (const sellIn of sellIns) {
      for (const quality of qualities) {
        const gildedRose = new GildedRose([new Item(name, sellIn, quality)])
        const acceptanceGildedRose = new AcceptanceGildedRose([new Item(name, sellIn, quality)])

        expect(gildedRose.updateQuality()).toEqual(acceptanceGildedRose.updateQuality())
        expect(gildedRose.items).toEqual(acceptanceGildedRose.items)
      }
    }
  }
})
