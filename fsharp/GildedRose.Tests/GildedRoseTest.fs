module GildedRoseTest

open GildedRose
open System
open System.IO
open System.Text
open NUnit.Framework
open System.Collections.Generic
open ApprovalTests
open ApprovalTests.Reporters

[<TestFixture>]
type GildedRoseTest () as this =
    [<Test>] member this.Foo ()=
        let Items = new List<Item>()  
        Items.Add({Name = "foo"; SellIn = 0; Quality = 0})
        let app = new GildedRose(Items)
        app.UpdateQuality()
        Assert.AreEqual("fixme", Items.[0].Name)

[<TestFixture>]
[<UseReporter(typeof<ApprovalTests.Reporters.NUnitReporter>)>]
type ApprovalTest () as this =
    [<Test>] member this.ThirtyDays ()=
        let fakeoutput = new StringBuilder()
        Console.SetOut(new StringWriter(fakeoutput))
        Console.SetIn(new StringReader("a\n"))

        main Array.empty<string>
        let output = fakeoutput.ToString()
        Approvals.Verify(output)
        ()