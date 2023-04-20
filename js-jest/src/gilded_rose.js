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

    updateQuality() {
        for (let i = 0; i < this.items.length; i++) {
            const item = this.items[i];
            if (item.name.toLowerCase().includes('aged brie')) {
                if(item.quality < 50) {
                    item.quality++;
                }
                item.sellIn--;
            } else if (item.name.toLowerCase().includes('backstage passes')) {
                if(item.sellIn === 0) {
                    item.quality = 0;
                } else if(item.sellIn >=1 && item.sellIn <= 3) {
                    const newValue = item.quality + 3;
                    item.quality = newValue > 50 ? 50 : newValue;
                } else if(item.sellIn >= 4 && item.sellIn <= 10) {
                    const newValue = item.quality + 2;
                    item.quality = newValue > 50 ? 50 : newValue;
                } else {
                    item.quality--;
                }
                item.sellIn--;
            } else if (item.name.toLowerCase().includes('sulfuras')) {
                // do nothing!
            } else if (item.name.toLowerCase().includes('conjured')) {
                const qualityDecrement = item.sellIn <=0 ? 4 : 2;
                const result = item.quality - qualityDecrement;
                if(result < 0) {
                    item.quality = 0;
                } else {
                    item.quality = result;
                }
                item.sellIn--;
            } else {
                // normal item
                const qualityDecrement = item.sellIn <=0 ? 2 : 1;
                const result = item.quality - qualityDecrement;
                if(result < 0) {
                    item.quality = 0;
                } else {
                    item.quality = result;
                }
                item.sellIn--;
            }
        }
        return this.items;
    }
}

module.exports = {
    Item,
    Shop
};
