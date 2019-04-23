module GildedRoseTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GildedRose exposing (Days(..), Quality(..), endOfDay, newItem)
import Test exposing (..)


suite : Test
suite =
    describe "GildedRose"
        [ describe "At the end of each day"
            [ test "our system lowers both values for every item" <|
                \_ ->
                    let
                        givenItems =
                            [ newItem (Days 10) (Quality 10) "Thing" ]

                        expectedItems =
                            [ newItem (Days 9) (Quality 9) "Thing" ]
                    in
                    Expect.equal expectedItems (endOfDay givenItems)
            , test "Once the sell by date has passed, Quality degrades twice as fast" <|
                \_ ->
                    let
                        givenItems =
                            [ newItem (Days 0) (Quality 10) "Thing" ]

                        expectedItems =
                            [ newItem (Days -1) (Quality 8) "Thing" ]
                    in
                    Expect.equal expectedItems (endOfDay givenItems)
            , test "The Quality of an item is never negative" <|
                \_ ->
                    let
                        givenItems =
                            [ newItem (Days 10) (Quality 0) "Thing" ]

                        expectedItems =
                            [ newItem (Days 9) (Quality 0) "Thing" ]
                    in
                    Expect.equal expectedItems (endOfDay givenItems)
            , test "'Aged Brie' actually increases in Quality the older it gets" <|
                \_ ->
                    let
                        givenItems =
                            [ newItem (Days 10) (Quality 0) "Aged Brie" ]

                        expectedItems =
                            [ newItem (Days 9) (Quality 1) "Aged Brie" ]
                    in
                    Expect.equal expectedItems (endOfDay givenItems)
            , test "The Quality of an item is never more than 50" <|
                \_ ->
                    let
                        givenItems =
                            [ newItem (Days 10) (Quality 50) "Aged Brie"
                            , newItem (Days 10) (Quality 100) "Thing"
                            ]

                        expectedItems =
                            [ newItem (Days 9) (Quality 50) "Aged Brie"
                            , newItem (Days 9) (Quality 50) "Thing"
                            ]
                    in
                    Expect.equal expectedItems (endOfDay givenItems)
            , test "\"Sulfuras\", being a legendary item, never has to be sold or decreases in Quality" <|
                \_ ->
                    let
                        givenItems =
                            [ newItem (Days 10) (Quality 80) "Sulfuras" ]

                        expectedItems =
                            [ newItem (Days 10) (Quality 80) "Sulfuras" ]
                    in
                    Expect.equal expectedItems (endOfDay givenItems)
            , describe "Backstage passes"
                [ test "like aged brie, increases in Quality as its SellIn value approaches" <|
                    \_ ->
                        let
                            givenItems =
                                [ newItem (Days 20) (Quality 0) "Backstage passes" ]

                            expectedItems =
                                [ newItem (Days 19) (Quality 1) "Backstage passes" ]
                        in
                        Expect.equal expectedItems (endOfDay givenItems)
                , test "Quality increases by 2 when there are 10 days or less; by 3 when there are 5 days or less; drops to 0 after the concert" <|
                    \_ ->
                        let
                            givenItems =
                                [ newItem (Days 11) (Quality 0) "Backstage passes"
                                , newItem (Days 10) (Quality 0) "Backstage passes"
                                , newItem (Days 9) (Quality 0) "Backstage passes"
                                , newItem (Days 6) (Quality 0) "Backstage passes"
                                , newItem (Days 5) (Quality 0) "Backstage passes"
                                , newItem (Days 4) (Quality 0) "Backstage passes"
                                , newItem (Days 1) (Quality 10) "Backstage passes"
                                , newItem (Days 0) (Quality 10) "Backstage passes"
                                , newItem (Days -1) (Quality 10) "Backstage passes"
                                ]

                            expectedItems =
                                [ newItem (Days 10) (Quality 1) "Backstage passes"
                                , newItem (Days 9) (Quality 2) "Backstage passes"
                                , newItem (Days 8) (Quality 2) "Backstage passes"
                                , newItem (Days 5) (Quality 2) "Backstage passes"
                                , newItem (Days 4) (Quality 3) "Backstage passes"
                                , newItem (Days 3) (Quality 3) "Backstage passes"
                                , newItem (Days 0) (Quality 13) "Backstage passes"
                                , newItem (Days -1) (Quality 0) "Backstage passes"
                                , newItem (Days -2) (Quality 0) "Backstage passes"
                                ]
                        in
                        Expect.equal expectedItems (endOfDay givenItems)
                ]
            ]
        ]
