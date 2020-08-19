//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

@testable import GildedRose
import XCTest

class SulfurasItemTests: XCTestCase {
    // MARK: - Test Cases for Sulfuras Items
    func testSulfurasItem() {
        let items = [  Item(name: "Sulfuras, Hand of Ragnaros", sellIn: 0, quality: 80),
                       Item(name: "Sulfuras, Hand of Ragnaros", sellIn: -1, quality: 80) ]
        
        let app = GildedRose(items: items)
        
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(0, app.items[0].sellIn) // Sulfuras Item is never sold. No change in Sell In
        XCTAssertEqual(-1, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(80, app.items[0].quality) // Sulfuras Item never changes the quality
        XCTAssertEqual(80, app.items[1].quality)
    }
}
