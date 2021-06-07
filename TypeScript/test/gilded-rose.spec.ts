import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';

describe('Gilded Rose', function () {

    it('should foo', function() {
        const gildedRose = new GildedRose([ new Item('foo', 0, 0) ]);
        const items = gildedRose.updateQuality();
        expect(items[0].name).to.equal('foo');
    });

});

describe('Gilded Rose', function () {
    it('Should give back updated SellIn', function () {
      const gildedRose = new GildedRose([ new Item('foo', 5, 5)]);
      const items = gildedRose.updateQuality();
      
      expect(items[0].sellIn).to.equal(4);
      expect(items[0].quality).to.equal(4);
    });
});

describe('Gilded Rose', function () {
    it('Should give back updated Quality', function () {
      const gildedRose = new GildedRose([ new Item('foo', 5, 5)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).to.equal(4);
    });
});




