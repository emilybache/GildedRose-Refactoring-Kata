const {ItemUpdater} = require("../item_updater");

class SulfurasUpdater extends ItemUpdater {
  updateQuality() {
    
    //The Quality is 80 and it never alters
    if (this.item.quality !== 80) {
      this.item.quality = 80;
    }
    return this.item;
  }
  }

  module.exports = {
    SulfurasUpdater
  }