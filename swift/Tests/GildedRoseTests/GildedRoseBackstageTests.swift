//
//  GildedRoseBackstageTests.swift
//
//
//  Created by Murat Bataray on 09/02/2023.
//

@testable import GildedRose
import XCTest

class GildedRoseBackstageTests: XCTestCase {
    func test_updateQuality_backstagePassQuality_shouldIncreaseOnceInQualityWhenMoreThan10DaysRemaining() throws {
        let items = [Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 15, quality: 1)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 2)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 3)
    }
    
    func test_updateQuality_backstagePassQuality_shouldIncreaseByTwoInQualityWhenLessThan10DaysRemaining() throws {
        let items = [Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 9, quality: 1)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 3)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 5)
    }
    
    func test_updateQuality_backstagePassQuality_shouldIncreaseByThreeInQualityWhenLessThan5DaysRemaining() throws {
        let items = [Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 5, quality: 1)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 4)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 7)
    }
    
    func test_updateQuality_backstagePassQuality_shouldBeZeroWhenSellByDateHasPassed() throws {
        let items = [Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 0, quality: 1)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 0)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 0)
    }
}

