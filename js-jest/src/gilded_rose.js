class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

class Rules {
  constructor(dailyQualityChangeValue, maxQuality, minQuality, tenDayChange, fiveDayChange,
    isZeroDayQualityDrop, zeroDayQualityValue) {
    this.dailyQualityChangeValue = dailyQualityChangeValue;
    this.maxQuality = maxQuality;
    this.minQuality = minQuality;
    this.tenDayChange = tenDayChange;
    this.fiveDayChange = fiveDayChange;
    this.isZeroDayQualityDrop = isZeroDayQualityDrop;
    this.zeroDayQualityValue = zeroDayQualityValue;
  }
}

class Shop {
  constructor(items = []) {
    this.items = items;
  }


  updateQuality() {
    const rules = {
      'Aged Brie': new Rules(1, 50, 0, 1, 1),
      'Backstage passes to a TAFKAL80ETC concert': new Rules(1, 50, 0)
    }
    for (let i = 0; i < this.items.length; i++) {
      if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
        if (this.items[i].quality > 0) {
          if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
            this.items[i].quality = this.items[i].quality - 1;
          }
        }
      } else {
        if (this.items[i].quality < 50) {
          this.items[i].quality = this.items[i].quality + 1;
          if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].sellIn < 11) {
              if (this.items[i].quality < 50) {
                this.items[i].quality = this.items[i].quality + 1;
              }
            }
            if (this.items[i].sellIn < 6) {
              if (this.items[i].quality < 50) {
                this.items[i].quality = this.items[i].quality + 1;
              }
            }
          }
        }
      }
      if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
        this.items[i].sellIn = this.items[i].sellIn - 1;
      }
      if (this.items[i].sellIn < 0) {
        if (this.items[i].name != 'Aged Brie') {
          if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
            if (this.items[i].quality > 0) {
              if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
                this.items[i].quality = this.items[i].quality - 1;
              }
            }
          } else {
            this.items[i].quality = this.items[i].quality - this.items[i].quality;
          }
        } else {
          if (this.items[i].quality < 50) {
            this.items[i].quality = this.items[i].quality + 1;
          }
        }
      }
    }

    return this.items;
  }
}

module.exports = {
  Item,
  Shop
}
