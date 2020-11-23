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
        XCTAssertEqual(QualityModificationRule .applyQualityThresholds(to: -1), 0)
        XCTAssertEqual(QualityModificationRule.applyQualityThresholds(to: 51), 50)
        XCTAssertEqual(QualityModificationRule.applyQualityThresholds(to: 24), 24)
    }

    func testIsRegularItemExpired() {
        
    }
}
