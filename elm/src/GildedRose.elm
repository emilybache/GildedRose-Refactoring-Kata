module GildedRose exposing (Days(..), Item(..), Quality(..), endOfDay, newItem)


type Quality
    = Quality Int


type Days
    = Days Int


type Item
    = RegularItem Days Quality String
    | BackstagePasses Days Quality
    | AgedBrie Days Quality
    | Sulfuras


newItem : Days -> Quality -> String -> Item
newItem (Days days) (Quality quality) name =
    if name == "Sulfuras" then
        Sulfuras

    else if name == "Backstage passes" then
        BackstagePasses (Days days) (Quality quality)

    else if name == "Aged Brie" then
        AgedBrie (Days days) (Quality quality)

    else
        RegularItem (Days days) (Quality quality) name


updateItem : Item -> Item
updateItem item =
    case item of
        RegularItem (Days days) (Quality quality) name ->
            if days <= 0 then
                RegularItem (Days (days - 1)) (Quality (min 50 (max 0 (quality - 2)))) name

            else
                RegularItem (Days (days - 1)) (Quality (min 50 (max 0 (quality - 1)))) name

        BackstagePasses (Days days) (Quality quality) ->
            if days <= 0 then
                BackstagePasses (Days (days - 1)) (Quality 0)

            else if days <= 5 then
                BackstagePasses (Days (days - 1)) (Quality (min 50 (quality + 3)))

            else if days <= 10 then
                BackstagePasses (Days (days - 1)) (Quality (min 50 (quality + 2)))

            else
                BackstagePasses (Days (days - 1)) (Quality (min 50 (quality + 1)))

        AgedBrie (Days days) (Quality quality) ->
            AgedBrie (Days (days - 1)) (Quality (min 50 (quality + 1)))

        Sulfuras ->
            item


endOfDay : List Item -> List Item
endOfDay itemList =
    List.map updateItem itemList
