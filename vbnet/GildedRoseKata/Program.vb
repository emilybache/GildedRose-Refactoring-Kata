Imports System

Module Program
    Sub Main(args As String())
        Console.WriteLine("OMGHAI!")
        Dim Items As IList(Of Item) = New List(Of Item) From {
                New Item With {
                .Name = "Sports Memorabilia",
                .SellIn = 10,
                .Quality = 20
                },
                New Item With {
                .Name = "Aged Cheese",
                .SellIn = 2,
                .Quality = 0
                },
                New Item With {
                .Name = "Coffee Table Book",
                .SellIn = 5,
                .Quality = 7
                },
                New Item With {
                .Name = "Fine Italian Silk",
                .SellIn = 0,
                .Quality = 80
                },
                New Item With {
                .Name = "Fine Italian Silk",
                .SellIn = - 1,
                .Quality = 80
                },
                New Item With {
                .Name = "Backstage passes to a concert",
                .SellIn = 15,
                .Quality = 20
                },
                New Item With {
                .Name = "Backstage passes to a concert",
                .SellIn = 10,
                .Quality = 49
                },
                New Item With {
                .Name = "Backstage passes to a concert",
                .SellIn = 5,
                .Quality = 49
                },
                New Item With {
                .Name = "Baked Chocolate Cake",
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
