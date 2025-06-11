import 'package:test/test.dart';
import 'package:gilded_rose/gilded_rose.dart';

main() {
  test('foo item', () {
    GildedRose app = GildedRose([Item('foo', 0, 0)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.quality, 0);
    expect(item.sellIn, -1);
    expect(item.name, "bar");
  });

  test('+5 Dexterity Vest', () {
    GildedRose app = GildedRose([Item('+5 Dexterity Vest', 10, 20)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 9);
    expect(item.quality, 19);
    expect(item.name, '+5 Dexterity Vest');
  });

  test('Aged Brie', () {
    GildedRose app = GildedRose([Item('Aged Brie', 2, 0)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 1);
    expect(item.quality, 1);
    expect(item.name, 'Aged Brie');
  });

  test('Elixir of the Mongoose', () {
    GildedRose app = GildedRose([Item('Elixir of the Mongoose', 5, 7)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 4);
    expect(item.quality, 6);
    expect(item.name, 'Elixir of the Mongoose');
  });

  test('Sulfuras, Hand of Ragnaros', () {
    GildedRose app = GildedRose([Item('Sulfuras, Hand of Ragnaros', 0, 80)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 0);
    expect(item.quality, 80);
    expect(item.name, 'Sulfuras, Hand of Ragnaros');
  });

  test('Sulfuras, Hand of Ragnaros - 2', () {
    GildedRose app = GildedRose([Item('Sulfuras, Hand of Ragnaros', -1, 80)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, -1);
    expect(item.quality, 80);
    expect(item.name, 'Sulfuras, Hand of Ragnaros');
  });

  test('Backstage passes to a TAFKAL80ETC concert', () {
    GildedRose app =
        GildedRose([Item('Backstage passes to a TAFKAL80ETC concert', 15, 20)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 14);
    expect(item.quality, 21);
    expect(item.name, 'Backstage passes to a TAFKAL80ETC concert');
  });

  test('Backstage passes to a TAFKAL80ETC concert - 2', () {
    GildedRose app =
        GildedRose([Item('Backstage passes to a TAFKAL80ETC concert', 10, 49)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 9);
    expect(item.quality, 50);
    expect(item.name, 'Backstage passes to a TAFKAL80ETC concert');
  });

  test('Backstage passes to a TAFKAL80ETC concert - 3', () {
    GildedRose app =
        GildedRose([Item('Backstage passes to a TAFKAL80ETC concert', 5, 49)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 4);
    expect(item.quality, 50);
    expect(item.name, 'Backstage passes to a TAFKAL80ETC concert');
  });

  test('Backstage passes to a TAFKAL80ETC concert - quality capped', () {
    GildedRose app =
        GildedRose([Item('Backstage passes to a TAFKAL80ETC concert', 5, 50)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 4);
    expect(item.quality, 50);
    expect(item.name, 'Backstage passes to a TAFKAL80ETC concert');
  });

  test('Backstage passes to a TAFKAL80ETC concert - value increase by two', () {
    GildedRose app =
        GildedRose([Item('Backstage passes to a TAFKAL80ETC concert', 9, 20)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 8);
    expect(item.quality, 22);
    expect(item.name, 'Backstage passes to a TAFKAL80ETC concert');
  });

  test('Backstage passes to a TAFKAL80ETC concert - value increase by three',
      () {
    GildedRose app =
        GildedRose([Item('Backstage passes to a TAFKAL80ETC concert', 5, 20)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 4);
    expect(item.quality, 23);
    expect(item.name, 'Backstage passes to a TAFKAL80ETC concert');
  });

  test('Conjured Mana Cake', () {
    GildedRose app = GildedRose([Item('Conjured Mana Cake', 3, 6)]);
    app.updateQuality();
    var item = app.items[0];

    expect(item.sellIn, 2);
    expect(item.quality, 4);
    expect(item.name, 'Conjured Mana Cake');
  });
}
