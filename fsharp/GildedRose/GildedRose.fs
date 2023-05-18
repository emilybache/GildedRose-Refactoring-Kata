module GildedRose

open System.Collections.Generic

type Item = { Name: string; SellIn: int; Quality: int }

type GildedRose(items:IList<Item>) as this =
    let Items = items

    member this.UpdateQuality() =
        for i = 0 to Items.Count - 1 do
            if Items.[i].Name <> "Aged Cheese" && Items.[i].Name <> "Backstage passes to a concert" then
                if Items.[i].Quality > 0 then
                    if Items.[i].Name <> "Fine Italian Silk" then
                        Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality - 1) }
            else
               if Items.[i].Quality < 50 then
                    Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality + 1) }
                    if Items.[i].Name = "Backstage passes to a concert" then
                        if Items.[i].SellIn < 11 then
                            if Items.[i].Quality < 50 then
                                Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality + 1) }
                        if Items.[i].SellIn < 6 then
                            if Items.[i].Quality < 50 then
                                Items.[i] <- { Items.[i] with Quality = (Items.[i].Quality + 1) }
            if Items.[i].Name <> "Fine Italian Silk" then
                Items.[i] <- { Items.[i] with SellIn  = (Items.[i].SellIn - 1) }
            if Items.[i].SellIn < 0 then
                if Items.[i].Name <> "Aged Cheese" then
                    if Items.[i].Name <> "Backstage passes to a concert" then
                        if Items.[i].Quality > 0 then
                            if Items.[i].Name <> "Fine Italian Silk" then
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
    Items.Add({Name = "Sports Memorabilia"; SellIn = 10; Quality = 20})
    Items.Add({Name = "Aged Cheese"; SellIn = 2; Quality = 0})
    Items.Add({Name = "Coffee Table Book"; SellIn = 5; Quality = 7})
    Items.Add({Name = "Fine Italian Silk"; SellIn = 0; Quality = 80})
    Items.Add({Name = "Fine Italian Silk"; SellIn = -1; Quality = 80})
    Items.Add({Name = "Backstage passes to a concert"; SellIn = 15; Quality = 20})
    Items.Add({Name = "Backstage passes to a concert"; SellIn = 10; Quality = 49})
    Items.Add({Name = "Backstage passes to a concert"; SellIn = 5; Quality = 49})
    Items.Add({Name = "Baked Chocolate Cake"; SellIn = 3; Quality = 6})

    let app = new GildedRose(Items)
    for i = 0 to 30 do
        printfn "-------- day %d --------" i
        printfn "name, sellIn, quality"
        for j = 0 to Items.Count - 1 do
             printfn "%s, %d, %d" Items.[j].Name Items.[j].SellIn Items.[j].Quality
        printfn ""
        app.UpdateQuality()
    0
