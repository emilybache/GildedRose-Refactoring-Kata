import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';

describe('Gilded Rose', function () {

    // quality of the normal items degrades by 1
    it('quality should degrades quality by 1', function() {
        const dataset = [
            new Item('item 1', 31, 12),
            new Item('item 2', 45, 21),
        ]

        const gildedRose = new GildedRose(dataset);
        const result = gildedRose.updateQuality();

        expect(result[0].name).to.equal('item 1');
        expect(result[0].sellIn).to.equal(30);
        expect(result[0].quality).to.equal(11);

        expect(result[1].name).to.equal('item 2');
        expect(result[1].sellIn).to.equal(44);
        expect(result[1].quality).to.equal(20);
    });
});
