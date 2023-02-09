//
//  GildedRoseConjuredTests.swift
//  
//
//  Created by Murat Bataray on 09/02/2023.
//

@testable import GildedRose
import XCTest

class GildedRoseConjuredTests: XCTestCase {
    func test_updateQuality_conjuredItemQuality_shouldDegradeTwiceAsFastAfterSellByDate() throws {
        let items = [Item(name: "Conjured item", sellIn: 1, quality: 15)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 13)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 9)
    }
}


