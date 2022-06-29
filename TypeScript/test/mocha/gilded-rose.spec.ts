import { expect } from 'chai';
import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose', () => {
  describe('Aged Brie', () => {
    it('one iteration, not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', 5, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(32);
    });

    it('one iteration, passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', -1, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(33);
    });

    it('multiple iterations, not reaching max quality and not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', 21, 31)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(41);
    });

    it('multiple iterations, not reaching max quality and passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', 5, 30)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(45);
    });

    it('multiple iterations, reaching max quality and passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Aged Brie', 5, 38)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(50);
    });
  });

  describe('Backstage passes to a TAFKAL80ETC concert', () => {
    it('one iteration, more than 10 days before selling, not reaching max quality', () => {
      const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 11, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(32);
    });

    it('one iteration, between 6 and 10 days before selling, not reaching max quality', () => {
      const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 10, 31),
                                               new Item('Backstage passes to a TAFKAL80ETC concert', 6, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(33);
      expect(gildedRose.items[1].quality).to.equal(33);
    });

    it('one iteration, 5 days or less before selling, not reaching max quality', () => {
      const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 5, 31),
                                               new Item('Backstage passes to a TAFKAL80ETC concert', 1, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(34);
      expect(gildedRose.items[1].quality).to.equal(34);
    });

    it('one iteration, passed sell date, not reaching max quality', () => {
      const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 0, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(0);
    });

    it('one iteration, reaching max quality', () => {
      const gildedRose = new GildedRose([new Item('Backstage passes to a TAFKAL80ETC concert', 3, 49)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(50);
    });

  });

  describe('Sulfuras, Hand of Ragnaros', () => {
    it('one iteration', () => {
      const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 5, 38)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(38);
    });

    it('multiple iterations, not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 22, 38)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(38);
    });

    it('multiple iterations, passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Sulfuras, Hand of Ragnaros', 5, 38)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(38);
    });
  });

  describe('Conjured item', () => {
    it('one iteration, not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Conjured', 5, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(29);
    });

    it('one iteration, passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Conjured', -1, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(27);
    });

    it('multiple iterations, not reaching negative quality and not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Conjured', 21, 31)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(11);
    });

    it('multiple iterations, not reaching negative quality and passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Conjured', 5, 37)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(7);
    });

    it('multiple iterations, reaching negative quality and passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Conjured', 5, 12)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(0);
    });
  });

  describe('Regular item', () => {
    it('one iteration, not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Any normal item', 5, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(30);
    });

    it('one iteration, passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Any normal item', -1, 31)]);
      gildedRose.updateQuality();
      expect(gildedRose.items[0].quality).to.equal(29);
    });

    it('multiple iterations, not reaching negative quality and not passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Any normal item', 21, 31)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(21);
    });

    it('multiple iterations, not reaching negative quality and passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Any normal item', 5, 30)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(15);
    });

    it('multiple iterations, reaching negative quality and passed sell date', () => {
      const gildedRose = new GildedRose([new Item('Any normal item', 5, 12)]);
      for (let i = 0; i < 10; ++i) {
        gildedRose.updateQuality();
      }
      expect(gildedRose.items[0].quality).to.equal(0);
    });
  });
});
