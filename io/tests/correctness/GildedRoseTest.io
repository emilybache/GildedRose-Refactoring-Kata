doRelativeFile("../../io/Item.io")
doRelativeFile("../../io/GildedRose.io")

GildedRoseTest := UnitTest clone do(

    testFoo := method(
        items := list( Item with("foo", 0, 0) )
        app := GildedRose with(items)
        app updateQuality
        assertEquals("fixme", app items at(0) name)
    )

)
