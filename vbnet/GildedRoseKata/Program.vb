Imports System

Module Program
    Sub Main(args As String())
        Console.WriteLine("OMGHAI!")
        Dim Items As IList(Of Item) = New List(Of Item) From {
                New Item With {
                .Name = "+5 Dexterity Vest",
                .SellIn = 10,
                .Quality = 20
                },
                New Item With {
                .Name = "Aged Brie",
                .SellIn = 2,
                .Quality = 0
                },
                New Item With {
                .Name = "Elixir of the Mongoose",
                .SellIn = 5,
                .Quality = 7
                },
                New Item With {
                .Name = "Sulfuras, Hand of Ragnaros",
                .SellIn = 0,
                .Quality = 80
                },
                New Item With {
                .Name = "Sulfuras, Hand of Ragnaros",
                .SellIn = - 1,
                .Quality = 80
                },
                New Item With {
                .Name = "Backstage passes to a TAFKAL80ETC concert",
                .SellIn = 15,
                .Quality = 20
                },
                New Item With {
                .Name = "Backstage passes to a TAFKAL80ETC concert",
                .SellIn = 10,
                .Quality = 49
                },
                New Item With {
                .Name = "Backstage passes to a TAFKAL80ETC concert",
                .SellIn = 5,
                .Quality = 49
                },
                New Item With {
                .Name = "Conjured Mana Cake",
                .SellIn = 3,
                .Quality = 6
                }
                }
        Dim app = New GildedRose(Items)

        For i = 0 To 31 - 1
            Console.WriteLine("-------- day " & i & " --------")
            Console.WriteLine("name, sellIn, quality")

            For j = 0 To Items.Count - 1
                System.Console.WriteLine(Items(j))
            Next

            Console.WriteLine("")
            app.UpdateQuality()
        Next
    End Sub
End Module
