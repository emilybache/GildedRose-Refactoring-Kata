
/**
 * Do not alter the Item class or Items property as those belong to the goblin in the corner who
 * will insta-rage and one-shot you as he doesn't believe in shared code ownership
 */
class Item {
  constructor (name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }
}

class RegularItem extends Item {
  constructor (itemProps) {
    const { name, sellIn, quality } = itemProps
    super(name, sellIn, quality)
    this.validateItemProps(itemProps)
  }

  _privateVar = 0

  get privateVar () {
    return this._privateVar
  }

  set privateVar (value) {
    console.log(`entered set privateVar with value: ${value}`)
    this._privateVar = value
  }

  validateItemProps ({ name, sellIn, quality }) {
    const errors = []

    const isNameValid = typeof name === 'string' && name.length
    const isSellInValid = typeof sellIn === 'number'
    // "The Quality of an item is never negative"
    // "The Quality of an item is never more than 50"
    const isQualityValid = typeof quality === 'number' && quality >= 0 && quality <= 50

    !isNameValid && errors.push('"name" must be a valid, non-empty string')
    !isSellInValid && errors.push('"sellIn" must be an integer')
    !isQualityValid && errors.push('"qualityValid" must be an integer, between 0 and 50')

    if (errors.length) {
      throw new Error(`[RegularItem.validateItemProps] Invalid itemProps passed to the constructor: ${errors.join(', ')}`)
    }
  }
}

class Shop {
  constructor (items = []) {
    this.items = items
  }

  updateQuality () {
    // Iterate list of items
    for (let i = 0; i < this.items.length; i++) {
      // NOT cheese && NOT pass && quality > 0 && NOT sulfuras, therefore RegularItem
      if (this.items[i].name !== 'Aged Brie' && this.items[i].name !== 'Backstage passes to a TAFKAL80ETC concert') {
        if (this.items[i].quality > 0) {
          if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
            // RegularItems decrement quality by 1
            this.items[i].quality = this.items[i].quality - 1
          }
        }
      } else {
        if (this.items[i].quality < 50) {
          // Increment quality by 1 for all non RegularItems
          this.items[i].quality = this.items[i].quality + 1

          if (this.items[i].name === 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].sellIn < 11) {
              if (this.items[i].quality < 50) {
                this.items[i].quality = this.items[i].quality + 1
              }
              // if sellIn is less than 11 for passes, net quality increase is 2
            }
            if (this.items[i].sellIn < 6) {
              if (this.items[i].quality < 50) {
                this.items[i].quality = this.items[i].quality + 1
              }
              // if sellIn is less than 6 for passes, net quality increase is 3
            }
          }
        }
      }

      // Unnecessary decrement of sellIn
      // "Sulfuras", being a legendary item, never has to be sold or decreases in Quality"
      if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
        this.items[i].sellIn = this.items[i].sellIn - 1
      }

      if (this.items[i].sellIn < 0) {
        if (this.items[i].name !== 'Aged Brie') {
          if (this.items[i].name !== 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].quality > 0) {
              if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
                // Not cheese, not pass, not sulfuras, therefore regular item
                this.items[i].quality = this.items[i].quality - 1
              }
            }
          } else {
            // Maybe pass (could be sulfuras), quality is zero because sellIn is negative
            this.items[i].quality = this.items[i].quality - this.items[i].quality
          }
        } else {
          // Is cheese, increment quality by 1 because sellIn is negative
          if (this.items[i].quality < 50) {
            this.items[i].quality = this.items[i].quality + 1
          }
        }
      }
    }

    return this.items
  }
}

module.exports = {
  Item,
  Shop,
  RegularItem
}
