//
//  AdvancedRules.swift
//  GildedRose
//
//  Created by Lucas van Dongen on 23/11/2020.
//

@testable import GildedRose
import XCTest

class AdvancedRules: XCTestCase {
    private let expiringItem = Item(name: "Expiring Item", sellIn: 1, quality: 5)
    private let noQualityItem = Item(name: "No quality Item", sellIn: 1, quality: 0)
    private let brieItem = Item(name: "Aged Brie", sellIn: 2, quality: 0)
    private let veryGoodBrieItem = Item(name: "Aged Brie", sellIn: 2, quality: 50)
    private let sulfuras = Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 0, quality: 50)

    //- Once the sell by date has passed, Quality degrades twice as fast
    func testQualityDegradationAfterExpire() {
        let initialQuality = expiringItem.quality

        let system = GildedRose(items: [expiringItem])
        system.updateQuality()

        let qualityAfterUpdateNotExpiredYet = system.items.first!.quality
        let qualityLossNotExpired = initialQuality - qualityAfterUpdateNotExpiredYet

        system.updateQuality()

        let qualityAfterUpdateExpired = system.items.first!.quality
        let qualityLossExpired = qualityAfterUpdateNotExpiredYet - qualityAfterUpdateExpired

        XCTAssertEqual(qualityLossNotExpired * 2, qualityLossExpired)
    }

    //- The Quality of an item is never negative
    func testQualityNeverNegative() {
        let system = GildedRose(items: [noQualityItem])
        system.updateQuality()

        let qualityAfterUpdate = system.items.first!.quality

        XCTAssertEqual(0, noQualityItem.quality)
        XCTAssertEqual(0, qualityAfterUpdate)
    }

    //- "Aged Brie" actually increases in Quality the older it gets
    func testBrieIncreasesInQuality() {
        let initialQuality = brieItem.quality
        let system = GildedRose(items: [brieItem])
        system.updateQuality()
        XCTAssertGreaterThan(system.items.first!.quality, initialQuality)
    }

    //- The Quality of an item is never more than 50
    func testQualityNeverOverMaximum() {
        let initialQuality = veryGoodBrieItem.quality
        let system = GildedRose(items: [veryGoodBrieItem])
        system.updateQuality()
        XCTAssertEqual(initialQuality, 50)
        XCTAssertEqual(system.items.first!.quality, 50)
    }
    
    //- "Sulfuras", being a legendary item, never has to be sold, never decreases in Quality
    func testLegendaryNoExpire() {
        let initialSellIn = sulfuras.sellIn
        let initialQuality = sulfuras.quality

        let system = GildedRose(items: [sulfuras])
        system.updateQuality()

        XCTAssertEqual(initialSellIn, system.items.first!.sellIn)
        XCTAssertEqual(initialQuality, system.items.first!.quality)
    }

    //- "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
    //  Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
    //  Quality drops to 0 after the concert
    func testBackstagePasses() {
        let backstagePassMoreThan10Days = Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 15, quality: 20)
        let backstagePassLessThan10Days = Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 10, quality: 20)
        let backstagePassLessThan5Days = Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 5, quality: 20)
        let backstagePassExpired = Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 0, quality: 20)

        XCTAssertTrue(backstagePassMoreThan10Days.updated(hasExpectedQuality: 21))
        XCTAssertTrue(backstagePassLessThan10Days.updated(hasExpectedQuality: 22))
        XCTAssertTrue(backstagePassLessThan5Days.updated(hasExpectedQuality: 23))
        XCTAssertTrue(backstagePassExpired.updated(hasExpectedQuality: 0))
    }
}
