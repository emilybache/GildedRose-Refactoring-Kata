module GildedRose exposing (Item, update_quality)


type alias Item =
    { name : String
    , sell_by : Int
    , quality : Int
    }


update_quality : List Item -> List Item
update_quality items =
    List.map
        (\item ->
            if item.name == "Aged Brie" || item.name == "Backstage passes to a TAFKAL80ETC concert" then
                if item.quality < 50 then
                    if item.name == "Backstage passes to a TAFKAL80ETC concert" then
                        if item.sell_by < 0 then
                            { item | sell_by = item.sell_by - 1, quality = 0 }

                        else if item.sell_by < 6 then
                            { item | sell_by = item.sell_by - 1, quality = item.quality + 3 }

                        else if item.sell_by < 11 then
                            { item | sell_by = item.sell_by - 1, quality = item.quality + 2 }

                        else
                            { item | sell_by = item.sell_by - 1, quality = item.quality + 1 }

                    else
                        { item | sell_by = item.sell_by - 1, quality = item.quality + 1 }

                else
                    { item | sell_by = item.sell_by }

            else if item.name /= "Aged Brie" && item.name /= "Sulfuras, Hand of Ragnaros" then
                if item.sell_by < 0 && item.quality > 0 then
                    if item.quality >= 2 then
                        { item | sell_by = item.sell_by - 1, quality = item.quality - 2 }

                    else
                        { item | sell_by = item.sell_by - 1, quality = 0 }

                else if item.quality >= 1 then
                    { item | sell_by = item.sell_by - 1, quality = item.quality - 1 }

                else
                    { item | sell_by = item.sell_by - 1, quality = 0 }

            else
                item
        )
        items
