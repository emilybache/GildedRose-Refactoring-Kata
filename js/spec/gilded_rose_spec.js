describe('Gilded Rose', function() {
  it('should foo', function() {
    const gildedRose = new Shop([new Item('foo', 0, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('foo');
  });

  it('can updateQuality for Backstage Passes', () => {
    const gildedRose = new Shop([
      new Item('Backstage passes to a TAFKAL80ETC concert', 15, 20),
      new Item('Backstage passes to a TAFKAL80ETC concert', 10, 46),
      new Item('Backstage passes to a TAFKAL80ETC concert', 5, 49)
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('Backstage passes to a TAFKAL80ETC concert');
    expect(items[0].sellIn).toEqual(14);
    expect(items[0].quality).toEqual(21);

    expect(items[1].name).toEqual('Backstage passes to a TAFKAL80ETC concert');
    expect(items[1].sellIn).toEqual(9);
    expect(items[1].quality).toEqual(48);

    expect(items[2].name).toEqual('Backstage passes to a TAFKAL80ETC concert');
    expect(items[2].sellIn).toEqual(4);
    expect(items[2].quality).toEqual(50);
  });

  it('can updateQuality for Aged Brie', () => {
    const gildedRose = new Shop([new Item('Aged Brie', 2, 0)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('Aged Brie');
    expect(items[0].sellIn).toEqual(1);
    expect(items[0].quality).toEqual(1);
  });

  it('can updateQuality for Sulfuras', () => {
    const gildedRose = new Shop([
      new Item('Sulfuras, Hand of Ragnaros', 0, 80),
      new Item('Sulfuras, Hand of Ragnaros', -1, 80)
    ]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('Sulfuras, Hand of Ragnaros');
    expect(items[0].sellIn).toEqual(-1);
    expect(items[0].quality).toEqual(80);

    expect(items[1].name).toEqual('Sulfuras, Hand of Ragnaros');
    expect(items[1].sellIn).toEqual(-2);
    expect(items[1].quality).toEqual(80);
  });

  it('can updateQuality for basic items', () => {
    const gildedRose = new Shop([
      new Item('+5 Dexterity Vest', 10, 20),
      new Item('Elixir of the Mongoose', 5, 7)
    ]);

    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('+5 Dexterity Vest');
    expect(items[0].sellIn).toEqual(9);
    expect(items[0].quality).toEqual(19);

    expect(items[1].name).toEqual('Elixir of the Mongoose');
    expect(items[1].sellIn).toEqual(4);
    expect(items[1].quality).toEqual(6);
  });

  it('can updateQuality for conjured items', () => {
    const gildedRose = new Shop([new Item('Conjured Mana Cake', 3, 6)]);
    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toEqual(2);
    expect(items[0].quality).toEqual(4);
  });

  it('Once the sell by date has passed, Quality degrades twice as fast', () => {
    const gildedRose = new Shop([new Item('+5 Dexterity Vest', -2, 20)]);

    const items = gildedRose.updateQuality();
    expect(items[0].sellIn).toEqual(-3);
    expect(items[0].quality).toEqual(18);
  });

  it('The Quality of an item is never negative', () => {
    const gildedRose = new Shop([new Item('+5 Dexterity Vest', -2, 1)]);

    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('+5 Dexterity Vest');
    expect(items[0].sellIn).toEqual(-3);
    expect(items[0].quality).toEqual(0);
  });

  it('will not increase quality over 50', () => {
    const gildedRose = new Shop([new Item('Aged Brie', -1, 50)]);
    const items = gildedRose.updateQuality();
    expect(items[0].name).toEqual('Aged Brie');
    expect(items[0].sellIn).toEqual(-2);
    expect(items[0].quality).toEqual(50);
  });
});
