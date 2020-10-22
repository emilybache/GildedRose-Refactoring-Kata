
import Table from 'cli-table'
import { Shop, ShopV2, Item } from '../src/gilded_rose'

/*
* "(...)do not alter the Item class or Items property as those belong to the goblin in the
* corner who will insta-rage and one-shot you as he doesn't believe in shared code ownership"
*/
const items = [
  new Item('+5 Dexterity Vest', 10, 20),
  new Item('Aged Brie', 2, 0),
  new Item('Elixir of the Mongoose', 5, 7),
  new Item('Sulfuras, Hand of Ragnaros', 0, 80),
  new Item('Sulfuras, Hand of Ragnaros', -1, 80),
  new Item('Backstage passes to a TAFKAL80ETC concert', 15, 20),
  new Item('Backstage passes to a TAFKAL80ETC concert', 10, 49),
  new Item('Backstage passes to a TAFKAL80ETC concert', 5, 49),

  // This Conjured item does not work properly yet
  new Item('Conjured Mana Cake', 3, 6)
]

const days = Number(process.argv[2]) || 2
const gildedRose = new Shop(items)
const gildedRoseV2 = new ShopV2(items)

for (let day = 0; day < days; day++) {
  const shopTable = new Table({
    head: ['Name', 'SellIn v1', 'Qlty v1', 'SellIn v2', 'Qlty v2'],
    colWidths: [43, 11, 9, 11, 9]
  })

  shopTable.push(...items.map(({ name, sellIn, quality }, index) => {
    const {
      sellIn: sellInV2,
      quality: qualityV2
    } = gildedRoseV2.items[index]
    return [name, sellIn, quality, sellInV2, qualityV2]
  }))

  console.log(`\n-------- Day ${day} --------`)
  console.log(shopTable.toString())
  gildedRose.updateQuality()
  gildedRoseV2.updateQuality()
}
