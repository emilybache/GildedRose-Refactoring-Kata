module Test.Generated.Main3492374619 exposing (main)

import GildedRoseTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "GildedRoseTest" [GildedRoseTest.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 387860746875023, processes = 8, paths = ["/Users/naomidennis/Documents/code-projects/elm/gilded_rose_elm/tests/GildedRoseTest.elm"]}