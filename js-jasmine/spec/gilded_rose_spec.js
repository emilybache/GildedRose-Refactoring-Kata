const {Shop, Item} = require('../src/gilded_rose.js');

const items = [
    new Item("+5 Dexterity Vest", 10, 20),
    new Item("Aged Brie", 2, 0),
    new Item("Elixir of the Mongoose", 5, 7),
    new Item("Sulfuras, Hand of Ragnaros", 0, 80),
    new Item("Sulfuras, Hand of Ragnaros", -1, 80),
    new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
    new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
    new Item("Conjured Mana Cake", 3, 6),
];

describe("Gilded Rose", function () {
    it("should calculate stock after 1 day", function () {
        const gildedRose = new Shop(deepCopy(items));
        const result = getStockAfterDays(gildedRose, items, 1);
        expect(result).toEqual([
            jasmine.objectContaining({name: '+5 Dexterity Vest', sellIn: 9, quality: 19}),
            jasmine.objectContaining({name: 'Aged Brie', sellIn: 1, quality: 1}),
            jasmine.objectContaining({name: 'Elixir of the Mongoose', sellIn: 4, quality: 6}),
            jasmine.objectContaining({name: 'Sulfuras, Hand of Ragnaros', sellIn: 0, quality: 80}),
            jasmine.objectContaining({name: 'Sulfuras, Hand of Ragnaros', sellIn: -1, quality: 80}),
            jasmine.objectContaining({name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 14, quality: 21}),
            jasmine.objectContaining({name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 9, quality: 50}),
            jasmine.objectContaining({name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 4, quality: 50}),
            jasmine.objectContaining({name: 'Conjured Mana Cake', sellIn: 2, quality: 4})
        ]);
    });

    it("should calculate stock after 2 days", function () {
        const gildedRose = new Shop(deepCopy(items));
        const result = getStockAfterDays(gildedRose, items, 2);
        expect(result).toEqual([
            jasmine.objectContaining({name: '+5 Dexterity Vest', sellIn: 8, quality: 18}),
            jasmine.objectContaining({name: 'Aged Brie', sellIn: 0, quality: 2}),
            jasmine.objectContaining({name: 'Elixir of the Mongoose', sellIn: 3, quality: 5}),
            jasmine.objectContaining({name: 'Sulfuras, Hand of Ragnaros', sellIn: 0, quality: 80}),
            jasmine.objectContaining({name: 'Sulfuras, Hand of Ragnaros', sellIn: -1, quality: 80}),
            jasmine.objectContaining({name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 13, quality: 22}),
            jasmine.objectContaining({name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 8, quality: 50}),
            jasmine.objectContaining({name: 'Backstage passes to a TAFKAL80ETC concert', sellIn: 3, quality: 50}),
            jasmine.objectContaining({name: 'Conjured Mana Cake', sellIn: 1, quality: 2})
        ]);
    });

    function getStockAfterDays(shop, originalItems, days) {
        return days ? [...Array(days)].map(() => shop.updateQuality())[days - 1] : originalItems;
    }

    function deepCopy(obj) {
      // Prevent original items from mutation and make tests independent
      // https://stackoverflow.com/a/53771927
        if (typeof obj !== 'object' || obj === null) {
            return obj;
        }

        if (obj instanceof Date) {
            return new Date(obj.getTime());
        }

        if (obj instanceof Array) {
            return obj.reduce((arr, item, i) => {
                arr[i] = deepCopy(item);
                return arr;
            }, []);
        }

        if (obj instanceof Object) {
            return Object.keys(obj).reduce((newObj, key) => {
                newObj[key] = deepCopy(obj[key]);
                return newObj;
            }, {})
        }
    }
});
