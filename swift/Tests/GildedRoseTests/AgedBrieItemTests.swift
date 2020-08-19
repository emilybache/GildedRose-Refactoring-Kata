//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

@testable import GildedRose
import XCTest

class AgedBrieItemTests: XCTestCase {
    // MARK: - Test Cases for Aged Brie Items
    func testAgedBrieItems() {
        let items = [
            Item(name: "Aged Brie", sellIn: 10, quality: 20),
            Item(name: "Aged Brie", sellIn: 2, quality: 0)]
        
        let app = GildedRose(items: items)
        
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(8, app.items[0].sellIn)
        XCTAssertEqual(0, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(22, app.items[0].quality)
        XCTAssertEqual(2, app.items[1].quality)
    }
    
    func testAgedBrieItemsWithQualityMoreThanFifty() {
        let items = [Item(name: "Aged Brie", sellIn: 5, quality: 49)]
        let app = GildedRose(items: items)
        let days = 4
        for _ in 0..<days {
            app.updateQuality()
        }
        // sellIn value
        XCTAssertEqual(1, app.items[0].sellIn)
        
        // quality value
        XCTAssertEqual(50, app.items[0].quality) // Quality of Aged Brie Item cannot be more than 50
    }
}
