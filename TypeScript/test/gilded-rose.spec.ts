import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';


// General Tests to ensure it works for newly created item
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

// tests to check if specific items work correctly 

describe('Backstage Pass', function () {
    it('Backstage pass should increase in quality by 2 with only 10 days left', function () {
        const gildedRose = new GildedRose ([ new Item('Backstage passes to a TAFKAL80ETC concert', 10, 5)]);
        const items = gildedRose.updateQuality();
        expect(items[0].quality).to.equal(7);
    })
})

describe('Backstage Pass', function () {
    it('Backstage pass should increase in quality by 3 with 5 days left', function () {
        const gildedRose = new GildedRose ([ new Item('Backstage passes to a TAFKAL80ETC concert', 5, 5)]);
        const items = gildedRose.updateQuality();
        expect(items[0].quality).to.equal(8);
    })
})


describe('Backstage Pass', function () {
    it('Backstage pass should increase in quality by 3, but quality is at 49', function () {
        const gildedRose = new GildedRose ([ new Item('Backstage passes to a TAFKAL80ETC concert', 4, 49)]);
        const items = gildedRose.updateQuality();
        expect(items[0].quality).to.equal(50);
    })
})

describe('Backstage Pass', function () {
    it('Backstage pass quality should drop to 0 after concert', function () {
        const gildedRose = new GildedRose ([ new Item('Backstage passes to a TAFKAL80ETC concert', 0, 50)]);
        const items = gildedRose.updateQuality();
        expect(items[0].quality).to.equal(0);
    })
})