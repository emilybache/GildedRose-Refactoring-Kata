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
    });
});

describe('Gilded Rose', function () {
    it('Should give back updated Quality', function () {
      const gildedRose = new GildedRose([ new Item('foo', 5, 5)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).to.equal(4);
    });
});

describe('Gilded Rose', function () {
    it('Quality should go down twice as much after sellIn date passes', function () {
      const gildedRose = new GildedRose([ new Item('foo', -1, 5)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).to.equal(3);
    });
});

describe('Gilded Rose', function () {
    it('Quality should not go negative', function () {
      const gildedRose = new GildedRose([ new Item('foo', -1, 0)]);
      const items = gildedRose.updateQuality();
      expect(items[0].quality).to.equal(0);
    });
});

// tests for backstage passes
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

// tests for aged brie
describe('Gilded Rose', () => {

    describe('Aged Brie Tests', () => {
        var gildedRose: GildedRose;
        
        beforeEach(() => {
            gildedRose = new GildedRose([new Item('Aged Brie', 7, 11)]);
        });
        
        it('Aged Brie should be added to item array', () => {
            const items = gildedRose.updateQuality()[0];
            expect(items.name).to.equal('Aged Brie');
        });
        
        it('should give back updated sellIn ', () => {
            const items = gildedRose.updateQuality()[0];
            expect(items.sellIn).to.equal(6);
        });
        
        it('should give back updated quality, Aged Brie increases in quality as days go on', () => {
            const items = gildedRose.updateQuality()[0];
            expect(items.quality).to.equal(12);
        });

        it('Aged Brie quality caps at 50', () => {
            gildedRose.items[0].quality = 50;
            const items = gildedRose.updateQuality()[0];
            expect(items.quality).to.equal(50);
        });
        
        it('Aged Brie quality continues to go up even after sellIn date passes', () => {
            gildedRose.items[0].sellIn = -12;
            const items = gildedRose.updateQuality()[0];
            expect(items.quality).to.equal(13);
        });
    });
}); 

// tests for hand of ragnaros 
describe('Gilded Rose', () => {

    describe('Sulfuras, Hand of Ragnaros Tests', () => {
        var gildedRose: GildedRose;
        
        beforeEach(() => {
            gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 1, 80)]);
        });
        
        it('Sulfuras, Hand of Ragnaros should be added to item array', () => {
            const items = gildedRose.updateQuality()[0];
            expect(items.name).to.equal('Sulfuras, Hand of Ragnaros');
        });
        
        it('Sulfuras, Hand of Ragnaros, should not have an updated sellIn ', () => {
            const items = gildedRose.updateQuality()[0];
            expect(items.sellIn).to.equal(1);
        });
        
        it('Sulfuras, Hand of Ragnaros quality should not change', () => {
            const items = gildedRose.updateQuality()[0];
            expect(items.quality).to.equal(80);
        });

        it('Sulfuras, Hand of Ragnaros should not have degraded quality even after sellIn date passes', () => {
            gildedRose.items[0].sellIn = -10;
            const items = gildedRose.updateQuality()[0];
            expect(items.quality).to.equal(80);
        });
    });
}); 

// tests for multiple items 
describe('Testing with multiple items at once', () => {
    var gildedRose: GildedRose;
    
    beforeEach(() => {
        gildedRose = new GildedRose([
            new Item('Sulfuras, Hand of Ragnaros', 1, 80),
            new Item('Bens Diary', 100, 1),
            new Item('Aged Brie', 7, 11),
            new Item('Wizards Wand', 1, 50),
            new Item('Wizards Hat', 20, 25),
            new Item('Kings Crown', 75, 45),
            new Item('Backstage passes to a TAFKAL80ETC concert', 10, 12),
        ]);
    });
    // Expect Sulfuras, Hand of Ragnaros sellIn to stay the same, all others to decrease
    it('should degrade all SellIn values on update', () => {
        const itemss = gildedRose.updateQuality();
        expect(itemss[0].sellIn).to.equal(1);
        expect(itemss[1].sellIn).to.equal(99);
        expect(itemss[2].sellIn).to.equal(6);
        expect(itemss[3].sellIn).to.equal(0);
        expect(itemss[4].sellIn).to.equal(19);
        expect(itemss[5].sellIn).to.equal(74);
        expect(itemss[6].sellIn).to.equal(9);
    });
    // expect Sulfuras, Hand of Ragnaros quality to stay the same, Aged brie and Concert tickets to increase, all else to decrease
    it('should degrade all Quality values on update', () => {
        const itemss = gildedRose.updateQuality();
        expect(itemss[0].quality).to.equal(80);
        expect(itemss[1].quality).to.equal(0);
        expect(itemss[2].quality).to.equal(12);
        expect(itemss[3].quality).to.equal(49);
        expect(itemss[4].quality).to.equal(24);
        expect(itemss[5].quality).to.equal(44);
        expect(itemss[6].quality).to.equal(14);
        
    });        
});