const { ItemUpdater } = require("../item_updater");

class BackStagePassesUpdater extends ItemUpdater {
    updateQuality() {

        //The Quality of an item is never more than 50
        if (this.item.quality > 50) {
            this.item.quality = 50;
        }

        if (this.item.sellIn <= 0) {
            this.qualityChangeFactor = 0;
        }

        if (this.item.quality > 0 && this.item.quality < 50) {

            if (this.item.sellIn <= 10) {
                this.qualityChangeFactor = 2;
            }

            if (this.item.sellIn <= 5) {
                this.qualityChangeFactor = 3;
            }
            this.item.quality = this.item.quality + this.qualityChangeFactor;
        }

        return this.item;
    }
}

module.exports = {
    BackStagePassesUpdater
}