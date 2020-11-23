//
//  BaseRules.swift
//  GildedRose
//
//  Created by Lucas van Dongen on 23/11/2020.
//

@testable import GildedRose
import XCTest

class BaseRules: XCTestCase {
    private let testItem = Item(name: "Test Item", sellIn: 3, quality: 5)

    //- All items have a SellIn value which denotes the number of days we have to sell the item
    func testSellInTest() {
        let itemMirror = Mirror(reflecting: testItem)
        let hasSellIn = itemMirror.children.contains { (child: (label: String?, _: Any)) -> Bool in
            child.label == "sellIn"
        }

        XCTAssertTrue(hasSellIn, "`sellIn` was not a member of `Item`")
    }

    //- All items have a Quality value which denotes how valuable the item is
    func testQualityValue() {
        let itemMirror = Mirror(reflecting: testItem)
        let hasSellIn = itemMirror.children.contains { (child: (label: String?, _: Any)) -> Bool in
            child.label == "quality"
        }

        XCTAssertTrue(hasSellIn, "`quality` was not a member of `Item`")
    }

    //- At the end of each day our system lowers both values for every item
    func testLowerValueEveryItem() {
        let initialSellIn = testItem.sellIn
        let initialQuality = testItem.quality

        let system = GildedRose(items: [testItem])
        system.updateQuality()

        XCTAssertLessThan(system.items.first!.sellIn, initialSellIn)
        XCTAssertLessThan(system.items.first!.quality, initialQuality)
    }
}
