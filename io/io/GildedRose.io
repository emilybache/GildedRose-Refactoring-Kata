GildedRose := Object clone do(

    init := method(
        self items := list()
    )

    with := method(items,
        result := self clone
        result items = items
        result
    )

    updateQuality := method(
        for(i, 0, items size - 1,
            if (items at(i) name != "Aged Brie" and
                items at(i) name != "Backstage passes to a TAFKAL80ETC concert",
                if (items at(i) quality > 0,
                    if (items at(i) name != "Sulfuras, Hand of Ragnaros",
                        items at(i) quality = items at(i) quality - 1
                    )
                )
            ,
                if (items at(i) quality < 50,
                    items at(i) quality = items at(i) quality + 1

                    if (items at(i) name == "Backstage passes to a TAFKAL80ETC concert",
                        if (items at(i) sellIn < 11,
                            if (items at(i) quality < 50,
                                items at(i) quality = items at(i) quality + 1
                            )
                        )

                        if (items at(i) sellIn < 6,
                            if (items at(i) quality < 50,
                                items at(i) quality = items at(i) quality + 1
                            )
                        )
                    )
                )
            )

            if (items at(i) name != "Sulfuras, Hand of Ragnaros",
                items at(i) sellIn = items at(i) sellIn - 1
            )

            if (items at(i) sellIn < 0,
                if (items at(i) name != "Aged Brie",
                    if (items at(i) name != "Backstage passes to a TAFKAL80ETC concert",
                        if (items at(i) quality > 0,
                            if (items at(i) name != "Sulfuras, Hand of Ragnaros",
                                items at(i) quality = items at(i) quality - 1
                            )
                        )
                    ,
                        items at(i) quality = items at(i) quality - items at(i) quality
                    )
                ,
                    if (items at(i) quality < 50,
                        items at(i) quality = items at(i) quality + 1
                    )
                )
            )
        )
    )
)
