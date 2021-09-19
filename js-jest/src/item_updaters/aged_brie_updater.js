const { ItemUpdater } = require("../item_updater");

class AgedBrieUpdater extends ItemUpdater {
    updateQuality() {

        //The Quality of an item is never more than 50
        if(this.item.quality > 50){
            this.item.quality= 50;
        }

        if (this.item.quality > 0 && this.item.quality < 50) {
            this.item.quality = this.item.quality +  this.qualityChangeFactor;
        }

        return this.item;
    }
}

module.exports = {
    AgedBrieUpdater
}