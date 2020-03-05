import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';

describe('Gilded Rose', function () {
    it('should reduce quality and sell date by 1', function() {
        const gildedRose = new GildedRose([ new Item('random_item', 1, 1) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('random_item');
        expect(items[0].sellIn).to.equal(0);
        expect(items[0].quality).to.equal(0);
    });

    it('should reduce quality by 2 after sell date', function() {
        const gildedRose = new GildedRose([ new Item('random_item', 0, 7) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('random_item');
        expect(items[0].sellIn).to.equal(-1);
        expect(items[0].quality).to.equal(5);
    });

    it('should reduce quality by 1 when it reaches sell date', function() {
        const gildedRose = new GildedRose([ new Item('random_item', 1, 7) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('random_item');
        expect(items[0].sellIn).to.equal(0);
        expect(items[0].quality).to.equal(6);
    });

    it('should not reduce quality when its already zero', function() {
        const gildedRose = new GildedRose([ new Item('random_item', 2, 0) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('random_item');
        expect(items[0].sellIn).to.equal(1);
        expect(items[0].quality).to.equal(0);
    });

    it('should increase in quality when the item is "Aged Brie"', function() {
        const gildedRose = new GildedRose([ new Item('Aged Brie', 2, 10) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Aged Brie');
        expect(items[0].sellIn).to.equal(1);
        expect(items[0].quality).to.equal(11);
    });

    it('should not have quality more than 50', function() {
        const gildedRose = new GildedRose([ new Item('Aged Brie', 2, 50) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Aged Brie');
        expect(items[0].sellIn).to.equal(1);
        expect(items[0].quality).to.equal(50);
    });

    it('should be able to have quality of 50', function() {
        const gildedRose = new GildedRose([ new Item('Aged Brie', 2, 49) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Aged Brie');
        expect(items[0].sellIn).to.equal(1);
        expect(items[0].quality).to.equal(50);
    });

    it('should never change the quality of Sulfuras', function() {
        const gildedRose = new GildedRose([ new Item('Sulfuras, Hand of Ragnaros', 2, 49) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Sulfuras, Hand of Ragnaros');
        expect(items[0].sellIn).to.equal(2);
        expect(items[0].quality).to.equal(49);
    });

    it('should increase in quality of Backstage passes by one when days more than 10', function() {
        const gildedRose = new GildedRose([ new Item('Backstage passes to a TAFKAL80ETC concert', 11, 40) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(items[0].sellIn).to.equal(10);
        expect(items[0].quality).to.equal(41);
    });

    it('should increase in quality of Backstage passes by two when days less than 11 and more than 5', function() {
        const gildedRose = new GildedRose([ new Item('Backstage passes to a TAFKAL80ETC concert', 10, 40) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(items[0].sellIn).to.equal(9);
        expect(items[0].quality).to.equal(42);
    });

    it('should increase in quality of Backstage passes by two when days are 6', function() {
        const gildedRose = new GildedRose([ new Item('Backstage passes to a TAFKAL80ETC concert', 6, 40) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(items[0].sellIn).to.equal(5);
        expect(items[0].quality).to.equal(42);
    });

    it('should increase in quality of Backstage passes by two when days are less than 5', function() {
        const gildedRose = new GildedRose([ new Item('Backstage passes to a TAFKAL80ETC concert', 5, 40) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(items[0].sellIn).to.equal(4);
        expect(items[0].quality).to.equal(43);
    });

    it('should set quality to zero when the concert has passed', function() {
        const gildedRose = new GildedRose([ new Item('Backstage passes to a TAFKAL80ETC concert', 0, 40) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(items[0].sellIn).to.equal(-1);
        expect(items[0].quality).to.equal(0);
    });
});
