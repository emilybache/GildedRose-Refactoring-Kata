//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

@testable import GildedRose
import XCTest

class StandardItemTests: XCTestCase {
    // MARK: - Test Cases for Standard Items
    func testStandardItemsForSellInDatePassed() {
        let items = [ Item(name: "Elixir of the Mongoose", sellIn: 1, quality: 7),
                      Item(name: "+5 Dexterity Vest", sellIn: 0, quality: 10)
        ]
        let app = GildedRose(items: items)
        let days = 3
        for _ in 0..<days {
            app.updateQuality()
        }
        // sellIn value
        XCTAssertEqual(-2, app.items[0].sellIn)
        XCTAssertEqual(-3, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(2, app.items[0].quality) // Item degrades twice in quality after sell date passed
        XCTAssertEqual(4, app.items[1].quality) // Quality never negative
    }
    
    func testStandardItemsWithQualityBelowZero() {
        let items = [Item(name: "+5 Dexterity Vest", sellIn: 3, quality: 1),
                     Item(name: "Elixir of the Mongoose", sellIn: 0, quality: 1)]
        let app = GildedRose(items: items)
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        // sellIn value
        XCTAssertEqual(1, app.items[0].sellIn)
        XCTAssertEqual(-2, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(0, app.items[0].quality) // Quality never negative
        XCTAssertEqual(0, app.items[1].quality)
    }
    
    func testStandardItemsForSellInDateNotPassed() {
        let items = [
            Item(name: "+5 Dexterity Vest", sellIn: 10, quality: 20),
            Item(name: "Elixir of the Mongoose", sellIn: 5, quality: 7)]
        let app = GildedRose(items: items)
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        // sellIn value
        XCTAssertEqual(8, app.items[0].sellIn)
        XCTAssertEqual(3, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(18, app.items[0].quality)
        XCTAssertEqual(5, app.items[1].quality)
    }
}
