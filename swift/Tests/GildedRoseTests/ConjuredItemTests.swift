//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

@testable import GildedRose
import XCTest

class ConjuredItemTests: XCTestCase {
    // MARK: -Test Cases for Conjured Items
    
    func testConjuredItems() {
        let items = [
            Item(name: "Conjured Mana Cake", sellIn: 3, quality: 50),
            Item(name: "Conjured Mana Cake", sellIn: 2, quality: 3)]
        
        let app = GildedRose(items: items)
        
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(1, app.items[0].sellIn)
        XCTAssertEqual(0, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(46, app.items[0].quality)
        XCTAssertEqual(0, app.items[1].quality)
    }
    
    func testConjuredItemsAfterSellInDatePassed() {
        let items = [ Item(name: "Conjured Mana Cake", sellIn: 0, quality: 25),
                      Item(name: "Conjured Mana Cake", sellIn: 1, quality: 27),
        ]
        
        let app = GildedRose(items: items)
        
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(-2, app.items[0].sellIn)
        XCTAssertEqual(-1, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(17, app.items[0].quality) //Quality of Conjured Items degrade twice as normal item
        XCTAssertEqual(21, app.items[1].quality)
    }
    
    func testConjuredItemsWithQualityBelowZero() {
        let items = [ Item(name: "Conjured Mana Cake", sellIn: 1, quality: 2),
                      Item(name: "Conjured Mana Cake", sellIn: -1, quality: 3),
        ]
        
        let app = GildedRose(items: items)
        
        let days = 2
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(-1, app.items[0].sellIn)
        XCTAssertEqual(-3, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(0, app.items[0].quality) //Quality of Conjured Items cannot be negative
        XCTAssertEqual(0, app.items[1].quality)
    }
}
