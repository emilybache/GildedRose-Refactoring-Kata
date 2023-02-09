//
//  GildedRoseBrieTests.swift
//  
//
//  Created by Murat Bataray on 09/02/2023.
//

@testable import GildedRose
import XCTest

class GildedRoseBrieTests: XCTestCase {
    func test_updateQuality_agedBrieQuality_increasesInQuality() throws {
        let items = [Item(name: "Aged Brie", sellIn: 1, quality: 4)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 5)
    }
    
    func test_updateQuality_agedBrieQuality_increasesInAgeAfterSellByDate() throws {
        let items = [Item(name: "Aged Brie", sellIn: 0, quality: 5)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 7)
    }
    
    func test_updateQuality_agedBrieQuality_shouldntGoOver50Quality() throws {
        let items = [Item(name: "Aged Brie", sellIn: 0, quality: 50)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 50)
    }
}

