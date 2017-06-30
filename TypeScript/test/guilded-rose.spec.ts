import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';

describe('Gilded Rose', function () {

    it('should foo', function() {
        const gilgedRose = new GildedRose([ new Item('foo', 0, 0) ]);
        const items = gilgedRose.updateQuality();
        expect(items[0].name).to.equal('fixme');
    });

});