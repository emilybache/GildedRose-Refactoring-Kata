Imports NUnit.Framework

Namespace GildedRoseKata.Tests
    Public Class GildedRoseTests
        <Test>
        Public Sub foo()
            Dim Items As IList(Of Item) = New List(Of Item) From {
                    New Item With {
                    .Name = "foo",
                    .SellIn = 0,
                    .Quality = 0
                    }
                    }
            Dim app = New GildedRose(Items)
            app.UpdateQuality()
            Assert.AreEqual("fixme", Items(0).Name)
        End Sub
    End Class
End Namespace