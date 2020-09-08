class Item {
    constructor(name, sellIn, quality) {
        this.name = name;
        this.sellIn = sellIn;
        this.quality = quality;
    }
}

class StandardItem extends Item {
    itemTomorrow() {
        this.quality = this.qualityTomorrow();
        this.sellIn--;
        return this;
    }

    qualityTomorrow() {
        let qualityTomorrow = this.quality - this.calculateDepreciation();
        if (qualityTomorrow < 0) {
            return 0;
        }
        if (qualityTomorrow >= 50) {
            return 50;
        }
        return qualityTomorrow;
    }

    calculateDepreciation() {
        return this.sellIn <= 0 ? 2 : 1;
    }
}

class Sulfuras extends Item {
    itemTomorrow() {
        return this;
    }
}

class ConjuredItem extends StandardItem {
    calculateDepreciation() {
        return this.sellIn <= 0 ? 4 : 2;
    }
}

class BackStagePass extends StandardItem {
    calculateDepreciation() {
        switch (true) {
            case this.sellIn <= 0:
                return this.quality;
            case this.sellIn <= 5:
                return -3;
            case this.sellIn <= 10:
                return -2;
            default:
                return -1;
        }
    }
}

class AgedBrie extends StandardItem {
    calculateDepreciation() {
        return this.sellIn <= 0 ? -2 : -1;
    }
}

class ItemTypeFactory {
    constructor() {
        this.setupItemTypes();
        this.setupClasses();
    }

    setupItemTypes() {
        this.itemTypes = new Map();
        this.itemTypes.set('Aged Brie', 'AgedBrie');
        this.itemTypes.set('Backstage passes', 'BackStagePass');
        this.itemTypes.set('Conjured', 'ConjuredItem');
        this.itemTypes.set('Sulfuras', 'Sulfuras');
        this.itemTypes.set('Standard', 'StandardItem');
    }

    setupClasses() {
        this.availableClasses = {
            StandardItem,
            Sulfuras,
            ConjuredItem,
            BackStagePass,
            AgedBrie
        };
    }

    getClassName(name) {
        const itemType = [...this.itemTypes]
            .filter(([key, value]) => name.includes(key))
            .map(([key, value]) => key)[0];
        return this.itemTypes.get(itemType || this.defaultClassName);
    }

    createItemUsingType(item) {
        const {name, sellIn, quality} = item;
        return new this.availableClasses[this.getClassName(name)](name, sellIn, quality);
    }

    get defaultClassName() {
        return 'Standard';
    }
}

class Shop {
    constructor(items = []) {
        const itemTypeFactory = new ItemTypeFactory();
        this.items = items.map((item) => itemTypeFactory.createItemUsingType(item));
    }

    updateQuality() {
        return this.items.map((item) => item.itemTomorrow());
    }
}

module.exports = {
    Item,
    Shop
}
