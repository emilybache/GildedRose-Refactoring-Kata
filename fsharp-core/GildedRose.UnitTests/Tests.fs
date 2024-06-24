module GildedRose.UnitTests

open GildedRose
open System.Collections.Generic
open Xunit
open Swensen.Unquote

[<Fact>]
let ``My test`` () =
    let Items = new List<Item>()  
    Items.Add({Name = "foo"; SellIn = 0; Quality = 0})
    let app = new GildedRose(Items)
    app.UpdateQuality()
    test <@ "fixme" = Items.[0].Name @>