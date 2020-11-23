//
//  TestSellInModificationRule.swift
//  GildedRose
//
//  Created by Lucas van Dongen on 23/11/2020.
//

@testable import GildedRose
import XCTest

class TestSellInModificationRule: XCTestCase {
    private let regularItem = Item(name: "Regular Item", sellIn: 1, quality: 5)
    private let backstagePassesItem = Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 1, quality: 0)
    private let brieItem = Item(name: "Aged Brie", sellIn: 2, quality: 0)
    private let sulfuras = Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 0, quality: 50)

    //- All items have a SellIn value which denotes the number of days we have to sell the item
    func testRuleSelection() {
        XCTAssertEqual(QualityModificationRule.rule(for: regularItem), .regular)
        XCTAssertEqual(QualityModificationRule.rule(for: backstagePassesItem), .qualityIncreasesFasterBeforeExpireThenDropsToZero)
        XCTAssertEqual(QualityModificationRule.rule(for: brieItem), .increasesQuality)
        XCTAssertEqual(QualityModificationRule.rule(for: sulfuras), .legendary)
    }

    func testApplyQualityThresholds() {
        let tooLow = Item(name: "Too low", sellIn: 0, quality: 0)
        let tooHigh = Item(name: "Too high", sellIn: 0, quality: 50)
        let unaltered = Item(name: "Too low", sellIn: 0, quality: 25)

        XCTAssertEqual(QualityModificationRule.applyQualityThresholds(to: tooLow, newQuality: -1).quality, 0)
        XCTAssertEqual(QualityModificationRule.applyQualityThresholds(to: tooHigh, newQuality: 51).quality, 50)
        XCTAssertEqual(QualityModificationRule.applyQualityThresholds(to: unaltered, newQuality: 24).quality, 24)
    }

    func testIsRegularItemExpired() {
        
    }
}
