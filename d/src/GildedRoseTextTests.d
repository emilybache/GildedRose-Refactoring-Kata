import GildedRose;

int main()
{
    import std.stdio : writefln, writeln;

    Item[] items = [
        Item("+5 Dexterity Vest", 10, 20),
        Item("Aged Brie", 2, 0),
        Item("Elixir of the Mongoose", 5, 7),
        Item("Sulfuras, Hand of Ragnaros", 0, 80),
        Item("Sulfuras, Hand of Ragnaros", -1, 80),
        Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
        // this Conjured item doesn't yet work properly
        Item("Conjured Mana Cake", 3, 6),
    ];

    auto app = new GildedRose(items);

    writeln("OMGHAI!");

    for (int day = 0; day <= 30; day++)
    {
        writefln!"-------- day %s --------"(day);
        writeln("Item(name, sellIn, quality)");
        foreach (item; app.items)
        {
            writeln(item);
        }
        writeln;

        app.updateQuality;
    }

    return 0;
}
