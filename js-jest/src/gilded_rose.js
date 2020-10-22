const AGED_CHEESE = ['Aged Brie']
const CONCERT_PASS = ['Backstage passes to a TAFKAL80ETC concert']
const LEGENDARY_ITEMS = ['Sulfuras, Hand of Ragnaros']
const CONJURED_ITEMS = ['Conjured Mana Cake']

/**
 * "(...) do not alter the Item class or Items property as those belong to the goblin in the corner
 * who will insta-rage and one-shot you as he doesn't believe in shared code ownership"
 */
export class Item {
  constructor (name, sellIn, quality) {
    this.name = name
    this.sellIn = sellIn
    this.quality = quality
  }
}

export class RegularItem extends Item {
  constructor (itemProps) {
    const { name, sellIn, quality } = itemProps
    super(name, sellIn, quality)
    this.validateItemProps(itemProps)
    this.depreciationRate = 1
  }

  /**
   * Item name validator
   *
   * @param {String} name
   * @returns {Boolean}
   */
  isNameValid (name) {
    return typeof name === 'string' && name.length
  }

  /**
   * Item "sellIn" validator
   *
   * @param {Number} sellIn
   * @returns {Boolean}
   */
  isSellInValid (sellIn) {
    return typeof sellIn === 'number'
  }

  /**
   * Item quality validator
   *
   * @param {Number} quality
   * @returns {Boolean}
   */
  isQualityValid (quality) {
    // "The Quality of an item is never negative"
    // "The Quality of an item is never more than 50"
    return typeof quality === 'number' && quality >= 0 && quality <= 50
  }

  /**
   * Item props validation to be used during initialization.
   *
   * @param {Object} itemProps
   */
  validateItemProps ({ name, sellIn, quality }) {
    const errors = []

    !this.isNameValid(name) && errors.push('"name" must be a valid, non-empty string')
    !this.isSellInValid(sellIn) && errors.push('"sellIn" must be an integer')
    !this.isQualityValid(quality) && errors.push('"quality" must be an integer, between 0 and 50')

    if (errors.length) {
      throw new Error(`[RegularItem.validateItemProps] Invalid itemProps passed to the constructor: ${errors.join(', ')}`)
    }
  }

  /**
   * "Once the sell by date has passed, Quality degrades twice as fast"
   *
   * @returns {Number}
   */
  getDepreciationRate () {
    return this.sellIn < 0 ? this.depreciationRate * 2 : this.depreciationRate
  }

  /**
   * "The Quality of an item is never more than 50"
   *
   * @param {Number} value
   */
  setQuality (value) {
    if (value > 50) {
      this.quality = 50
    } else {
      this.quality = value
    }
  }

  /**
   * Updates this item's quality, ensuring it is never a negative number.
   * Decrements the item's sellIn property.
   */
  updateQuality () {
    // "The Quality of an item is never negative"
    this.setQuality(Math.max(0, this.quality - this.getDepreciationRate()))

    // Assuming updateQuality is called only once a day...
    // "At the end of each day our system lowers both values [quality and sellIn] (...)"
    this.sellIn--
  }
}

export class ConcertPass extends RegularItem {
  constructor (itemProps) {
    super(itemProps)

    // "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches
    this.depreciationRate = -1
  }

  /**
   * Calculates a concert pass' depreciation rate according to how close the concert is.
   */
  getDepreciationRate () {
    // "Quality drops to 0 after the concert"
    if (this.sellIn < 0) {
      return this.quality
    }

    // [Quality increases] by 3 when there are 5 days or less"
    if (this.sellIn <= 5) {
      return this.depreciationRate * 3
    }

    // "[and] by 2 when there are 10 days or less"
    if (this.sellIn <= 10) {
      return this.depreciationRate * 2
    }

    return this.depreciationRate
  }
}

export class AgedCheese extends RegularItem {
  constructor (itemProps) {
    super(itemProps)

    // "Aged Brie" actually increases in Quality the older it gets
    this.depreciationRate = -1
  }

  /**
   * Calculates the depreciation rate of an aged cheese, which should be inverted (appreciation).
   * For cheese, the legacy logic shows 0 sellIn is inclusive when determining the acceleration of
   * the appreciation rate (twice as fast past its "sellIn" date).
   */
  getDepreciationRate () {
    return this.sellIn <= 0 ? this.depreciationRate * 2 : this.depreciationRate
  }
}

export class LegendaryItem extends RegularItem {
  constructor (itemProps) {
    super(itemProps)
    this.depreciationRate = 0
  }

  /**
   * "(...) an item can never have its Quality increase above 50, however (...) a legendary
   * item['s quality] is 80 and it never alters."
   *
   * @param {Number} quality
   * @returns {Boolean}
   */
  isQualityValid (quality) {
    return typeof quality === 'number' && quality === 80
  }

  /**
   * "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
   */
  updateQuality () {}
}

export class ConjuredItem extends RegularItem {
  constructor (itemProps) {
    super(itemProps)

    // "Conjured" items degrade in Quality twice as fast as normal items
    this.depreciationRate = 2
  }
}

export class Shop {
  constructor (items = []) {
    /*
    * "(...)do not alter the Item class or Items property as those belong to the goblin in the
    * corner who will insta-rage and one-shot you as he doesn't believe in shared code ownership"
    */
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
          // Increment quality by 1 for all non RegularItems (cheese and concert pass)
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

      if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
        // Decrement sellIn each time updateQuality is called
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

export class ShopV2 extends Shop {
  constructor (items = []) {
    // Copying the items fulfills the constraint of not mutating the Items list, and adds new
    // functionality to each item according to the provided requirements.
    const itemsV2 = items.map(({ name, sellIn, quality }) => {
      let ItemClass = RegularItem

      // Special Items

      if (LEGENDARY_ITEMS.indexOf(name) !== -1) {
        ItemClass = LegendaryItem
      }

      if (AGED_CHEESE.indexOf(name) !== -1) {
        ItemClass = AgedCheese
      }

      if (CONCERT_PASS.indexOf(name) !== -1) {
        ItemClass = ConcertPass
      }

      if (CONJURED_ITEMS.indexOf(name) !== -1) {
        ItemClass = ConjuredItem
      }

      return new ItemClass({ name, sellIn, quality })
    })

    super(itemsV2)
  }

  updateQuality () {
    this.items.forEach((item) => {
      item.updateQuality()
    })
  }
}
