class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

class Shop {
  constructor(items = []) {
    this.items = items;
  }
  /*  
  反轉負向判定（!）
  中斷程式碼簡化判定
  整合判定（sellIn 變動提到前面拉齊比對基準）
  各商品行為抽成單獨的 function
  updateQuality => updateItem？
  */

  updateQuality() {
    for (let i = 0; i < this.items.length; i++) {
      if (
        this.items[i].name === "Aged Brie" ||
        this.items[i].name === "Backstage passes to a TAFKAL80ETC concert"
      ) {
        if (this.items[i].quality < 50) {
          this.items[i].quality = this.items[i].quality + 1;
          if (
            this.items[i].name == "Backstage passes to a TAFKAL80ETC concert"
          ) {
            if (this.items[i].sellIn < 11 && this.items[i].quality < 50) {
              this.items[i].quality = this.items[i].quality + 1;
            }
            if (this.items[i].sellIn < 6 && this.items[i].quality < 50) {
              this.items[i].quality = this.items[i].quality + 1;
            }
          }
        }
      } else if (
        this.items[i].quality > 0 &&
        this.items[i].name != "Sulfuras, Hand of Ragnaros"
      ) {
        this.items[i].quality = this.items[i].quality - 1;
      }

      if (this.items[i].name != "Sulfuras, Hand of Ragnaros") {
        this.items[i].sellIn = this.items[i].sellIn - 1;
      }

      if (this.items[i].sellIn < 0) {
        if (this.items[i].name != "Aged Brie") {
          if (
            this.items[i].name != "Backstage passes to a TAFKAL80ETC concert"
          ) {
            if (
              this.items[i].quality > 0 &&
              this.items[i].name != "Sulfuras, Hand of Ragnaros"
            ) {
              this.items[i].quality = this.items[i].quality - 1;
            }
          } else {
            this.items[i].quality =
              this.items[i].quality - this.items[i].quality;
          }
        } else if (this.items[i].quality < 50) {
          this.items[i].quality = this.items[i].quality + 1;
        }
      }
    }

    return this.items;
  }
}

module.exports = {
  Item,
  Shop,
};
