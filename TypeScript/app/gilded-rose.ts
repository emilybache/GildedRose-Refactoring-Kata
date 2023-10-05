import { Item } from "./components/Item";
import { ItemConstants } from "./constants/constant";

export class GildedRose {
  items: Array<Item>;

  constructor(items = [] as Array<Item>) {
    this.items = items;
  }

  /**
   * Function to update quality
   */
  updateQuality(){
    for (let i = 0; i < this.items.length; i++) {
      const item = this.items[i];

      // Check for specific item types and update quality accordingly
      switch (item.name) {
        case ItemConstants.AGED_BRIE:
          item.sellIn--;

          //"Aged Brie" actually increases in Quality the older it gets
          item.quality = Math.min(item.quality + 1, ItemConstants.MAX_QUALITY);
          break;

        case ItemConstants.BACKSTAGE:
          item.sellIn--;

          // The Quality of an item is never negative
          if (item.sellIn < 0) {
            item.quality = 0;
          } else if (item.sellIn <= 5) {
            // by 3 when there are 5 days or less but
            item.quality = Math.min(item.quality + 3, ItemConstants.MAX_QUALITY);
          } else if (item.sellIn <= 10) {
            // Quality increases by 2 when there are 10 days or less
            item.quality = Math.min(item.quality + 2, ItemConstants.MAX_QUALITY);
          } else {
            item.quality = Math.min(item.quality + 1, ItemConstants.MAX_QUALITY);
          }
          break;
        case ItemConstants.SULFURAS:
          // "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
          break;
        case ItemConstants.CONJURED:
          item.sellIn--;

          // "Conjured" items degrade in Quality twice as fast as normal items
          item.quality = Math.max(item.quality - 2, 0);
          break;
        default:
          // Normal item
          item.sellIn--;
          item.quality = Math.max(item.quality - 1, 0);

          // The Quality of an item is never negative
          if (item.sellIn < 0) {
            item.quality = Math.max(item.quality - 1, 0);
          }
          break;
      }
    }  

    return this.items;
  }

  // updateQualityOld() {
  //   for (let i = 0; i < this.items.length; i++) {
  //     if (this.items[i].name != ItemConstants.AGED_BRIE && this.items[i].name != ItemConstants.BACKSTAGE) {
  //       if (this.items[i].quality > 0) {
  //         if (this.items[i].name != ItemConstants.SULFURAS) {
  //           this.items[i].quality = this.items[i].quality - 1
  //         }
  //       }
  //     } else {
  //       if (this.items[i].quality < 50) {
  //         this.items[i].quality = this.items[i].quality + 1
  //         if (this.items[i].name == ItemConstants.BACKSTAGE) {
  //           if (this.items[i].sellIn < 11) {
  //             if (this.items[i].quality < 50) {
  //               this.items[i].quality = this.items[i].quality + 1
  //             }
  //           }
  //           if (this.items[i].sellIn < 6) {
  //             if (this.items[i].quality < 50) {
  //               this.items[i].quality = this.items[i].quality + 1
  //             }
  //           }
  //         }
  //       }
  //     }
  //     if (this.items[i].name != ItemConstants.SULFURAS) {
  //       this.items[i].sellIn = this.items[i].sellIn - 1;
  //     }
  //     if (this.items[i].sellIn < 0) {
  //       if (this.items[i].name != ItemConstants.AGED_BRIE) {
  //         if (this.items[i].name != ItemConstants.BACKSTAGE) {
  //           if (this.items[i].quality > 0) {
  //             if (this.items[i].name != ItemConstants.SULFURAS) {
  //               this.items[i].quality = this.items[i].quality - 1
  //             }
  //           }
  //         } else {
  //           this.items[i].quality = this.items[i].quality - this.items[i].quality
  //         }
  //       } else {
  //         if (this.items[i].quality < 50) {
  //           this.items[i].quality = this.items[i].quality + 1
  //         }
  //       }
  //     }
  //   }

  //   return this.items;
  // }
}
