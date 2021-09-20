import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';

const updateQuality = (dataset:Item[]) => new GildedRose(dataset).updateQuality();

describe('Gilded Rose', function () {

    // quality of the normal items degrades by 1
    it('quality should degrades quality by 1', function() {
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
    it('quality should degrades twice as fast', function() {
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

});
