//
//  GildedRoseSulfurasTests.swift
//  
//
//  Created by Murat Bataray on 09/02/2023.
//

@testable import GildedRose
import XCTest

class GildedRoseSulfurasTests: XCTestCase {
    func test_updateQuality_sulfurasQuality_shouldntHaveSellDate() throws {
        let items = [Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 0, quality: 1)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 1)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 1)
    }

    func test_updateQuality_sulfurasQuality_shouldntDecreaseInQuality() throws {
        let items = [Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 1, quality: 1)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 1)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 1)
    }

}
