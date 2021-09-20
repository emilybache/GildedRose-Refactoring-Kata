import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';

const updateQuality = (dataset:Item[]) => new GildedRose(dataset).updateQuality();

describe('Gilded Rose', function () {

    // quality of the normal items degrades by 1
    it('should degrades quality by 1', function() {
        const dataset = [
            new Item('item 1', 31, 12),
            new Item('item 2', 45, 21),
        ]

        const result = updateQuality(dataset);

        expect(result[0].name).to.equal('item 1');
        expect(result[0].sellIn).to.equal(30);
        expect(result[0].quality).to.equal(11);

        expect(result[1].name).to.equal('item 2');
        expect(result[1].sellIn).to.equal(44);
        expect(result[1].quality).to.equal(20);
    });

    // Once the sell by date has passed, Quality degrades twice as fast
    it('should degrades quality twice as fast', function() {
        const dataset = [
            new Item('item 1', 0, 10),
            new Item('item 2', -5, 17),
        ]

        const result = updateQuality(dataset);

        expect(result[0].name).to.equal('item 1');
        expect(result[0].sellIn).to.equal(-1);
        expect(result[0].quality).to.equal(8);

        expect(result[1].name).to.equal('item 2');
        expect(result[1].sellIn).to.equal(-6);
        expect(result[1].quality).to.equal(15);
    });

    // "Aged Brie" actually increases in Quality the older it gets
    it('should increases quality by 1 for "Aged Brie"', function() {
        const dataset = [
            new Item('Aged Brie', 5, 15),
            new Item('Aged Brie', 10, 12),
            new Item('Aged Brie', 0, 18),
        ]

        const result = updateQuality(dataset);

        expect(result[0].name).to.equal('Aged Brie');
        expect(result[0].sellIn).to.equal(4);
        expect(result[0].quality).to.equal(16);

        expect(result[1].name).to.equal('Aged Brie');
        expect(result[1].sellIn).to.equal(9);
        expect(result[1].quality).to.equal(13);

        expect(result[2].name).to.equal('Aged Brie');
        expect(result[2].sellIn).to.equal(-1);
        /* 
         * TODO: need clarifications, as of the requirement doc expected result should be 19 
         * but quality increases twice when input SellIn <= 0. 
         */
        expect(result[2].quality).to.equal(20); 
    });

    // The Quality of an item is never more than 50
    it('should quality never more than 50', function() {
        const dataset = [
            new Item('Aged Brie', 10, 49),
            new Item('Aged Brie', 7, 50),
            new Item('Backstage passes to a TAFKAL80ETC concert', 8, 49),
            new Item('Backstage passes to a TAFKAL80ETC concert', 3, 50),
        ]

        const result = updateQuality(dataset);

        expect(result[0].name).to.equal('Aged Brie');
        expect(result[0].sellIn).to.equal(9);
        expect(result[0].quality).to.equal(50);

        expect(result[1].name).to.equal('Aged Brie');
        expect(result[1].sellIn).to.equal(6);
        expect(result[1].quality).to.equal(50);

        expect(result[2].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[2].sellIn).to.equal(7);
        expect(result[2].quality).to.equal(50);

        expect(result[3].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[3].sellIn).to.equal(2);
        expect(result[3].quality).to.equal(50);
    });

     // "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
     it('should legendary item, never has to be changed', function() {
        const dataset = [
            new Item('Sulfuras, Hand of Ragnaros', 10, 80),
            new Item('Sulfuras, Hand of Ragnaros', 0, 80),
        ]

        const result = updateQuality(dataset);

        expect(result[0].name).to.equal('Sulfuras, Hand of Ragnaros');
        expect(result[0].sellIn).to.equal(10);
        expect(result[0].quality).to.equal(80);

        expect(result[1].name).to.equal('Sulfuras, Hand of Ragnaros');
        expect(result[1].sellIn).to.equal(0);
        expect(result[1].quality).to.equal(80);
    });

    /* 
     * "Backstage passes", Quality increases by 2 when there are 10 days or less 
     * "Backstage passes", Quality increases by 3 when there are 5 days or less
     * "Backstage passes", Quality drops to 0 after the concert
     */
    it('should increases quality value depends on the SellIn value for "Backstage passes"', function() {
        const dataset = [
            new Item('Backstage passes to a TAFKAL80ETC concert', 10, 43),
            new Item('Backstage passes to a TAFKAL80ETC concert', 7, 24),
            new Item('Backstage passes to a TAFKAL80ETC concert', 5, 22),
            new Item('Backstage passes to a TAFKAL80ETC concert', 3, 35),
            new Item('Backstage passes to a TAFKAL80ETC concert', 0, 15),
        ]

        const result = updateQuality(dataset);

        expect(result[0].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[0].sellIn).to.equal(9);
        expect(result[0].quality).to.equal(45);

        expect(result[1].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[1].sellIn).to.equal(6);
        expect(result[1].quality).to.equal(26);

        expect(result[2].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[2].sellIn).to.equal(4);
        expect(result[2].quality).to.equal(25);

        expect(result[3].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[3].sellIn).to.equal(2);
        expect(result[3].quality).to.equal(38);

        expect(result[4].name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        expect(result[4].sellIn).to.equal(-1);
        expect(result[4].quality).to.equal(0);
    });
});
