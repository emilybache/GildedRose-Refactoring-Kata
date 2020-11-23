//
//  TestQualityModificationRules.swift
//  GildedRose
//
//  Created by Lucas van Dongen on 23/11/2020.
//

@testable import GildedRose
import XCTest

class TestQualityModificationRules: XCTestCase {
    private let regularItem = Item(name: "Regular Item", sellIn: 1, quality: 5)
    private let sulfuras = Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 0, quality: 50)

    //- All items have a SellIn value which denotes the number of days we have to sell the item
    func testRuleSelection() {
        XCTAssertEqual(SellInModificationRule.rule(for: regularItem), .regular)
        XCTAssertEqual(SellInModificationRule.rule(for: sulfuras), .legendary)
    }

    func testApplyLegendary() {
        let initialSellIn = sulfuras.sellIn
        let system = GildedRose(items: [sulfuras])
        system.updateQuality()

        XCTAssertEqual(system.items.first!.sellIn, initialSellIn)
    }

    func testApplyRegular() {
        let initialSellIn = regularItem.sellIn
        let system = GildedRose(items: [regularItem])
        system.updateQuality()

        XCTAssertLessThan(system.items.first!.sellIn, initialSellIn)
    }
}
