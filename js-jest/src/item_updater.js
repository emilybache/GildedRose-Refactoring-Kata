class ItemUpdater {
    constructor(item) {
      this.item = item;
      this.qualityChangeFactor = 1;
    }
  
    updateQuality() {
  
      //Once the sell by date has passed, Quality degrades twice as fast
      if(this.item.sellIn < 0) {
        this.qualityChangeFactor = 2;
      }
  
      //The Quality of an item is never more than 50
      if (this.item.quality > 50) {
        this.item.quality = 50;
      }
  
      if (this.item.quality > 0 && this.item.quality < 50) {
        this.item.quality = this.item.quality - this.qualityChangeFactor;
      }
      return this.item;
    }
  }

  module.exports = {
    ItemUpdater
  }