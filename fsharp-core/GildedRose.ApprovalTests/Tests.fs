module GildedRose.ApprovalTests

open System
open Xunit
open System.Text
open System.IO

[<Fact>]
let ``Thirty day report is correct`` () =
    let expected = File.ReadAllText "GildedRoseTest.ApprovalTest.ThirtyDays.received.txt"

    let fakeoutput = new StringBuilder()
    Console.SetOut(new StringWriter(fakeoutput))
    Console.SetIn(new StringReader("a\n"))

    GildedRose.Program.main [||] |> ignore
    let actual = fakeoutput.ToString()

    Assert.Equal(expected, actual)
