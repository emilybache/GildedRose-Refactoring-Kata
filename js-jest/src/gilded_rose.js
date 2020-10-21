
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

  validateItemProps({ name, sellIn, quality }) {
    const errors = [];

    const isNameValid = typeof name === 'string' && name.length
    const isSellInValid = typeof sellIn === 'number' && sellIn > 0
    const isQualityValid = typeof quality === 'number' && quality >= 0 && quality <=50

    !isNameValid && errors.push('"name" must be a valid, non-empty string')
    !isSellInValid && errors.push('"sellIn" must be an integer, greater than zero')
    !isQualityValid && errors.push('"qualityValid" must be an integer, between 0 and 50')

    if (errors.length) {
      throw new Error(`[RegularItem.validateItemProps] Invalid itemProps passed to the constructor: ${errors.join(', ')}`)
    }
  }
}

class Blade extends Item {
  constructor(...args) {
    super(...args)
  }

  whatAmI() {
    console.log(`[Blade] I am a Blade with: name: ${this.name}, sellIn: ${this.sellIn}, quality: ${this.quality}`)
  }
}

class Sword extends Blade {
  constructor(...args) {
    super(...args)
  }

  whoAmI() {
    console.log('[Sword] I am a sword! AND...')
    this.whatAmI()
  }
}

class Shop {
  constructor (items = []) {
    this.items = items
  }

  updateQuality () {
    for (let i = 0; i < this.items.length; i++) {
      if (this.items[i].name !== 'Aged Brie' && this.items[i].name !== 'Backstage passes to a TAFKAL80ETC concert') {
        if (this.items[i].quality > 0) {
          if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
            this.items[i].quality = this.items[i].quality - 1
          }
        }
      } else {
        if (this.items[i].quality < 50) {
          this.items[i].quality = this.items[i].quality + 1
          if (this.items[i].name === 'Backstage passes to a TAFKAL80ETC concert') {
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
      if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
        this.items[i].sellIn = this.items[i].sellIn - 1
      }
      if (this.items[i].sellIn < 0) {
        if (this.items[i].name !== 'Aged Brie') {
          if (this.items[i].name !== 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].quality > 0) {
              if (this.items[i].name !== 'Sulfuras, Hand of Ragnaros') {
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

module.exports = {
  Item,
  Shop,
  Sword,
  RegularItem
}
