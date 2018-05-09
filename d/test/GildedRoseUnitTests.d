import GildedRose;

unittest
{
    Item[] items = [ Item("Foo", 0, 0)];
    auto app = new GildedRose(items);

    app.updateQuality;

    assert("fixme" == app.items[0].name);
}

void example()
{
    Item[] items = [
        Item("+5 Dexterity Vest", 10, 20),
        Item("Aged Brie", 2, 0),
        Item("Elixir of the Mongoose", 5, 7),
        Item("Sulfuras, Hand of Ragnaros", 0, 80),
        Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        Item("Conjured Mana Cake", 3, 6),
    ];
    auto app = new GildedRose(items);
    app.updateQuality;
}
