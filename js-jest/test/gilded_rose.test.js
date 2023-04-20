const {Shop, Item} = require("../src/gilded_rose");
const Chance = require('chance');

describe("Gilded Rose", function() {
  let chance;

  beforeEach(() => {
    chance = new Chance();
  });

  it('should decrement quality and sellIn', () => {
    const quality = chance.integer({min: 1, max: 50});
    const sellIn = chance.integer({min: 1});

    const gildedRose = new Shop([new Item("foo", sellIn, quality)]);
    const items = gildedRose.updateQuality();

    expect(items[0].name).toBe("foo");
    expect(items[0].sellIn).toBe(sellIn - 1);
    expect(items[0].quality).toBe(quality - 1);
  });

  it('should never have a negative quality', () => {
    const gildedRose = new Shop([new Item("foo", 0, 0)]);
    const items = gildedRose.updateQuality();

    expect(items[0].name).toBe("foo");
    expect(items[0].sellIn).toBe(-1);
    expect(items[0].quality).toBe(0);
  });


  describe('backstage passes', () => {
    it('should drop quality to 0 after sell date', () => {
      const quality = chance.integer({min: 4, max: 50});

      const gildedRose = new Shop([new Item("Backstage passes", 0, quality)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(sellIn - 1);
      expect(items[0].quality).toBe(quality + 3);
    });

    it.only('should increase quality by 2 when there are between 4 and 10 days to sell', () => {
      const quality = chance.integer({min: 1, max: 48});
      const sellIn = chance.integer({min: 4, max: 10});

      const gildedRose = new Shop([new Item("Backstage passes", sellIn, quality)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(sellIn - 1);
      expect(items[0].quality).toBe(quality + 2);
    });

    it('should increase quality by 3 when there are less than 4 days to sell date', () => {
      const quality = chance.integer({min: 1, max: 47});
      const sellIn = chance.integer({min: 1, max: 3});

      const gildedRose = new Shop([new Item("backstage passes", sellIn, quality)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(sellIn - 1);
      expect(items[0].quality).toBe(quality + 3);
    });

    it('should not exceed 50 in quality', () => {
      const sellIn = chance.integer({min: 1, max: 3});

      const gildedRose = new Shop([new Item("Backstage passes", sellIn, 49)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(sellIn - 1);
      expect(items[0].quality).toBe(50);
    });
  });

  describe('sulfuras', () => {
    it('should have no sell by date and quality does not change', () => {
      const gildedRose = new Shop([new Item("Sulfuras", null, 80)]);

      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(null);
      expect(items[0].quality).toBe(80);
    })
  });

  describe('aged brie', () => {
    it('should increase by quality as it gets older', () => {
      const quality = chance.integer({min: 1, max: 49});
      const sellIn = chance.integer({min: 1});

      const gildedRose = new Shop([new Item("Aged Brie", sellIn, quality)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(sellIn - 1);
      expect(items[0].quality).toBe(quality + 1);
    });

    it('should not exceed 50 quality', () => {
      const sellIn = chance.integer({min: 1});

      const gildedRose = new Shop([new Item("Aged Brie", sellIn, 50)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(sellIn - 1);
      expect(items[0].quality).toBe(50);
    });
  });

  describe('after sell by date', () => {
      it('should decrease quality twice as fast', () => {
        const quality = chance.integer({min: 2, max: 50});

        const gildedRose = new Shop([new Item("foo", 0, quality)]);
        const items = gildedRose.updateQuality();

        expect(items[0].sellIn).toBe(-1);
        expect(items[0].quality).toBe(quality - 2);
      });
  });

  describe('conjured items', () => {
      it('should degrade quality twice as fast as normal items', () => {
        const quality = chance.integer({min: 2, max: 50});
        const sellIn = chance.integer({min: 1});

        const gildedRose = new Shop([new Item("Conjured", sellIn, quality)]);
        const items = gildedRose.updateQuality();

        expect(items[0].sellIn).toBe(sellIn - 1);
        expect(items[0].quality).toBe(quality - 2);
      });

      it('should degrade quality twice as fast as normal expired items', () => {
        const quality = chance.integer({min: 4, max: 50});
        const sellIn = chance.integer({max: 0});

        const gildedRose = new Shop([new Item("Conjured", sellIn, quality)]);
        const items = gildedRose.updateQuality();

        expect(items[0].sellIn).toBe(sellIn - 1);
        expect(items[0].quality).toBe(quality - 4);
      });

    it('should never have a negative quality', () => {
      const gildedRose = new Shop([new Item("Conjured", 0, 0)]);
      const items = gildedRose.updateQuality();

      expect(items[0].sellIn).toBe(-1);
      expect(items[0].quality).toBe(0);
    });

  });



});
