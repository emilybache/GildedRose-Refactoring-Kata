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
        Item("Sports Memorabilia", 10, 20),
        Item("Aged Cheese", 2, 0),
        Item("Coffee Table Book", 5, 7),
        Item("Fine Italian Silk", 0, 80),
        Item("Backstage passes to a concert", 15, 20),
        Item("Baked Chocolate Cake", 3, 6),
    ];
    auto app = new GildedRose(items);
    app.updateQuality;
}
