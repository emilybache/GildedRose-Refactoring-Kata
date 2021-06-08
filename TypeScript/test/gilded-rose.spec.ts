import { expect } from 'chai';
import { Item, GildedRose } from '../app/gilded-rose';


// General Tests to ensure it works for newly created item
describe('Gilded Rose', () => {

    describe('General tests', () => {
        var gildedRose: GildedRose;
        
        beforeEach(() => {
            gildedRose = new GildedRose([new Item('foo', 5, 5)]);
        });
        
        it('Foo should be added to item array', () => {
            const items = gildedRose.items[0];
            expect(items.name).to.equal('foo');
        });
        
        it('should give back updated sellIn ', () => {
            const items = gildedRose.items[0];
            expect(items.sellIn).to.equal(4);
        });
        
        it('should give back updated quality', () => {
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(4);
        });

        
        it('Quality decreases twice as fast after sellIn date', () => {
            gildedRose.items[0].sellIn = -1;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(3);
        });

        it('Quality should not go negative', () => {
            gildedRose.items[0].quality = 0;
            gildedRose.items[0].sellIn = -1;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(0);
        });
    });
}); 

// tests for backstage passes
describe('Gilded Rose', () => {

    describe('Backstage passes to a TAFKAL80ETC concert Tests', () => {
        var gildedRose: GildedRose;
        
        beforeEach(() => {
            gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 10, 5)]);
        });
        
        it('Backstage passes to a TAFKAL80ETC concert should be added to item array', () => {
            const items = gildedRose.items[0];
            expect(items.name).to.equal('Backstage passes to a TAFKAL80ETC concert');
        });
        
        it('should give back updated sellIn ', () => {
            const items = gildedRose.items[0];
            expect(items.sellIn).to.equal(9);
        });
        
        it('should give back updated quality, Backstage passes increase in quality as days go on', () => {
            gildedRose.items[0].sellIn = 12;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(6);
        });

        it('Backstage pass quality should increase by 2 with 10 or less days', () => {
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(7);
        });

        it('Backstage pass quality should increase by 3 with 5 or less days', () => {
            gildedRose.items[0].sellIn = 2;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(8);
        });

        it('Backstage Pass quality drops to 0 after concert', () => {
            gildedRose.items[0].sellIn = -1;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(0);
        });

        it('Backstage Pass quality caps at 50', () => {
            gildedRose.items[0].quality = 50;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(50);
        });
    });
}); 

// tests for aged brie
describe('Gilded Rose', () => {

    describe('Aged Brie Tests', () => {
        var gildedRose: GildedRose;
        
        beforeEach(() => {
            gildedRose = new GildedRose([new Item('Aged Brie', 7, 11)]);
        });
        
        it('Aged Brie should be added to item array', () => {
            const items = gildedRose.items[0];
            expect(items.name).to.equal('Aged Brie');
        });
        
        it('should give back updated sellIn ', () => {
            const items = gildedRose.items[0];
            expect(items.sellIn).to.equal(6);
        });
        
        it('should give back updated quality, Aged Brie increases in quality as days go on', () => {
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(12);
        });

        it('Aged Brie quality caps at 50', () => {
            gildedRose.items[0].quality = 50;
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(50);
        });
        
        it('Aged Brie quality continues to go up even after sellIn date passes', () => {
            gildedRose.items[0].sellIn = -12;
            const items = gildedRose.items[0];
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
            const items = gildedRose.items[0];
            expect(items.name).to.equal('Sulfuras, Hand of Ragnaros');
        });
        
        it('Sulfuras, Hand of Ragnaros, should not have an updated sellIn ', () => {
            const items = gildedRose.items[0];
            expect(items.sellIn).to.equal(1);
        });
        
        it('Sulfuras, Hand of Ragnaros quality should not change', () => {
            const items = gildedRose.items[0];
            expect(items.quality).to.equal(80);
        });

        it('Sulfuras, Hand of Ragnaros should not have degraded quality even after sellIn date passes', () => {
            gildedRose.items[0].sellIn = -10;
            const items = gildedRose.items[0];
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
        const items = gildedRose.updateQuality();
        expect(items[0].sellIn).to.equal(1);
        expect(items[1].sellIn).to.equal(99);
        expect(items[2].sellIn).to.equal(6);
        expect(items[3].sellIn).to.equal(0);
        expect(items[4].sellIn).to.equal(19);
        expect(items[5].sellIn).to.equal(74);
        expect(items[6].sellIn).to.equal(9);
    });
    // expect Sulfuras, Hand of Ragnaros quality to stay the same, Aged brie and Concert tickets to increase, all else to decrease
    it('should degrade all Quality values on update', () => {
        const items = gildedRose.updateQuality();
        expect(items[0].quality).to.equal(80);
        expect(items[1].quality).to.equal(0);
        expect(items[2].quality).to.equal(12);
        expect(items[3].quality).to.equal(49);
        expect(items[4].quality).to.equal(24);
        expect(items[5].quality).to.equal(44);
        expect(items[6].quality).to.equal(14);
        
    });        
});