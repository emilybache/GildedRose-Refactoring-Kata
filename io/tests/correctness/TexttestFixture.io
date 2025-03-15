doRelativeFile("../../io/Item.io")
doRelativeFile("../../io/GildedRose.io")

writeln("OMGHAI!")

items := list(
    Item with("+5 Dexterity Vest", 10, 20), //
    Item with("Aged Brie", 2, 0), //
    Item with("Elixir of the Mongoose", 5, 7), //
    Item with("Sulfuras, Hand of Ragnaros", 0, 80), //
    Item with("Sulfuras, Hand of Ragnaros", -1, 80),
    Item with("Backstage passes to a TAFKAL80ETC concert", 15, 20),
    Item with("Backstage passes to a TAFKAL80ETC concert", 10, 49),
    Item with("Backstage passes to a TAFKAL80ETC concert", 5, 49),
    // this conjured item does not work properly yet
    Item with("Conjured Mana Cake", 3, 6)
)

app := GildedRose with(items)

days := 2
if (System args size > 1,
    days = System args at(1) asNumber + 1
)

for(i, 0, days - 1,
    writeln("-------- day " .. i .. " --------")
    writeln("name, sellIn, quality")
    items foreach(item,
        writeln(item)
    )
    writeln
    app updateQuality
)
