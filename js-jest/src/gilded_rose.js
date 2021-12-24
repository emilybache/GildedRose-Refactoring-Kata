class Item {
  constructor(name, sellIn, quality) {
    this.name = name;
    this.sellIn = sellIn;
    this.quality = quality;
  }
}

//This class will hold the rules for the items. making it easier to accomodate future additions to the shop
class Rules {
  constructor(dailyQualityChangeValue, maxQuality, minQuality, tenDayChange, fiveDayChange,
    isZeroDayQualityDrop, zeroDayQualityValue, isSellInValueChangeAllowed) {
    this.dailyQualityChangeValue = dailyQualityChangeValue;
    this.maxQuality = maxQuality;
    this.minQuality = minQuality;
    this.tenDayChange = tenDayChange;
    this.fiveDayChange = fiveDayChange;
    this.isZeroDayQualityDrop = isZeroDayQualityDrop;
    this.zeroDayQualityValue = zeroDayQualityValue;
    this.isSellInValueChangeAllowed = isSellInValueChangeAllowed;
  }
}

class Shop {
  constructor(items = []) {
    this.items = items;
  }


  updateQuality() {
    const rules = {
      'Aged Brie': new Rules(1, 50, 0, 1, 1, false, 0, true),
      'Backstage passes to a TAFKAL80ETC concert': new Rules(1, 50, 0, 2, 3, true, 0, true),
      'Sulfuras, Hand of Ragnaros': new Rules(0, Infinity, 0, 0, 0, false, 0, false),
      'Conjured Mana Cake': new Rules(-2, 50, 0, -2, -2, false, 0, true),
      'DEFAULT': new Rules(-1, 50, 0, -1, -1, false, 0, true)
    }
    //1. Iterate over the item list
    for (let i = 0; i < this.items.length; i++) {
      let itemName = this.items[i].name;
      //2. Check rules object and fetch corresponding rules. If the item name is not found in rules use 'DEFAULT' rule
      let itemRule;
      if (rules[itemName]) {
        itemRule = rules[itemName];
      } else {
        itemRule = rules['DEFAULT'];
      }
      // console.log("Rules for item " + itemName + " is " + JSON.stringify(itemRule));
      this.applyRule(this.items[i], itemRule);
    }

    return this.items;
  }

  applyRule(itemObj, ruleObj) {
    if (itemObj.sellIn <= 0 && !ruleObj.isZeroDayQualityDrop) {
      //if sell in day is less than 0 then add the quality by 2 times dailyChangeValue 
      //Not applicable for concert tickets aka zero day quality drop items
      itemObj.quality = itemObj.quality + 2 * ruleObj.dailyQualityChangeValue;
    } else if (itemObj.sellIn <= 0 && ruleObj.isZeroDayQualityDrop) {
      /*
      if sell in day is 0 and quality drop is required on 0 day
      then assign zero day value to quality
      */
      itemObj.quality = ruleObj.zeroDayQualityValue;

      //Decrement sell in and return
      if (ruleObj.isSellInValueChangeAllowed) {
        itemObj.sellIn = itemObj.sellIn - 1;
      }
      return;

    } else if (itemObj.sellIn <= 5) {
      /*
      if sell in day is 5 or less then add fiveDayChange value to quality
      */
      itemObj.quality = itemObj.quality + ruleObj.fiveDayChange;
    } else if (itemObj.sellIn <= 10) {
      /*
      if sell in day is 10 or less then add tenDayChange value to quality
      */
      itemObj.quality = itemObj.quality + ruleObj.tenDayChange;
    } else {
      /*
      for any other sellIn day add dailyQualityChangeValue to quality
      */
      itemObj.quality = itemObj.quality + ruleObj.dailyQualityChangeValue;
    }

    //Check Quality value min and max limits and adjust
    this.applyQualityMaxMinCheck(itemObj, ruleObj.minQuality, ruleObj.maxQuality)

    //decrease sell in value by 1
    if (ruleObj.isSellInValueChangeAllowed) {
      itemObj.sellIn = itemObj.sellIn - 1;
    }


  }

  applyQualityMaxMinCheck(itemObj, minVal, maxVal) {
    //Check and update items quality based on max and min values allowed
    if (itemObj.quality < minVal) {
      itemObj.quality = minVal;
    } else if (itemObj.quality > maxVal) {
      itemObj.quality = maxVal;
    }
  }

  // updateQuality() {
  //   for (let i = 0; i < this.items.length; i++) {
  //     if (this.items[i].name != 'Aged Brie' && this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
  //       if (this.items[i].quality > 0) {
  //         if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
  //           this.items[i].quality = this.items[i].quality - 1;
  //         }
  //       }
  //     } else {
  //       if (this.items[i].quality < 50) {
  //         this.items[i].quality = this.items[i].quality + 1;
  //         if (this.items[i].name == 'Backstage passes to a TAFKAL80ETC concert') {
  //           if (this.items[i].sellIn < 11) {
  //             if (this.items[i].quality < 50) {
  //               this.items[i].quality = this.items[i].quality + 1;
  //             }
  //           }
  //           if (this.items[i].sellIn < 6) {
  //             if (this.items[i].quality < 50) {
  //               this.items[i].quality = this.items[i].quality + 1;
  //             }
  //           }
  //         }
  //       }
  //     }
  //     if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
  //       this.items[i].sellIn = this.items[i].sellIn - 1;
  //     }
  //     if (this.items[i].sellIn < 0) {
  //       if (this.items[i].name != 'Aged Brie') {
  //         if (this.items[i].name != 'Backstage passes to a TAFKAL80ETC concert') {
  //           if (this.items[i].quality > 0) {
  //             if (this.items[i].name != 'Sulfuras, Hand of Ragnaros') {
  //               this.items[i].quality = this.items[i].quality - 1;
  //             }
  //           }
  //         } else {
  //           this.items[i].quality = this.items[i].quality - this.items[i].quality;
  //         }
  //       } else {
  //         if (this.items[i].quality < 50) {
  //           this.items[i].quality = this.items[i].quality + 1;
  //         }
  //       }
  //     }
  //   }

  //   return this.items;
  // }


}

module.exports = {
  Item,
  Shop
}
