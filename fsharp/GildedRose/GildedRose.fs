module GildedRose

open System.Collections.Generic

type Item = { Name: string; SellIn: int; Quality: int }

type GildedRose(items:IList<Item>) as this =
    let Items = items

    member this.UpdateQuality() =
        for i = 0 to Items.Count - 1 do
            if Items.[i].Name <> "Aged Brie" && Items.[i].Name <> "Backstage passes to a TAFKAL80ETC concert" then
                if Items.[i].Quality > 0 then
                    if Items.[i].Name <> "Sulfuras, Hand of Ragnaros" then
                        Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality - 1) } 
            else
               if Items.[i].Quality < 50 then
                    Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality + 1) } 
                    if Items.[i].Name = "Backstage passes to a TAFKAL80ETC concert" then
                        if Items.[i].SellIn < 11 then
                            if Items.[i].Quality < 50 then
                                Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality + 1) } 
                        if Items.[i].SellIn < 6 then
                            if Items.[i].Quality < 50 then
                                Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality + 1) } 
            if Items.[i].Name <> "Sulfuras, Hand of Ragnaros" then                 
                Items.[i] <- { Items.[i] with SellIn  = (Items.[i].SellIn - 1) } 
            if Items.[i].SellIn < 0 then
                if Items.[i].Name <> "Aged Brie" then
                    if Items.[i].Name <> "Backstage passes to a TAFKAL80ETC concert" then
                        if Items.[i].Quality > 0 then
                            if Items.[i].Name <> "Sulfuras, Hand of Ragnaros" then
                                Items.[i] <- { Items.[i] with Quality   = (Items.[i].Quality  - 1) } 
                    else
                        Items.[i] <- { Items.[i] with Quality   = (Items.[i].Quality  - Items.[i].Quality) } 
                else
                    if Items.[i].Quality < 50 then
                        Items.[i] <- { Items.[i] with Quality   = (Items.[i].Quality + 1) }  
        ()

[<EntryPoint>]
let main argv = 
    printfn "OMGHAI!"
    let Items = new List<Item>()
    Items.Add({Name = "+5 Dexterity Vest"; SellIn = 10; Quality = 20})
    Items.Add({Name = "Aged Brie"; SellIn = 2; Quality = 0})
    Items.Add({Name = "Elixir of the Mongoose"; SellIn = 5; Quality = 7})
    Items.Add({Name = "Sulfuras, Hand of Ragnaros"; SellIn = 0; Quality = 80})
    Items.Add({Name = "Sulfuras, Hand of Ragnaros"; SellIn = -1; Quality = 80})
    Items.Add({Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 15; Quality = 20})
    Items.Add({Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 10; Quality = 49})
    Items.Add({Name = "Backstage passes to a TAFKAL80ETC concert"; SellIn = 5; Quality = 49})
    Items.Add({Name = "Conjured Mana Cake"; SellIn = 3; Quality = 6})

    let app = new GildedRose(Items)
    for i = 0 to 30 do
        printfn "-------- day %d --------" i
        printfn "name, sellIn, quality"
        for j = 0 to Items.Count - 1 do
             printfn "%s, %d, %d" Items.[j].Name Items.[j].SellIn Items.[j].Quality
        printfn ""
        app.UpdateQuality()
    0 