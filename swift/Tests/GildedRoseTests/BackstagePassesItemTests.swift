//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

@testable import GildedRose
import XCTest

class BackstagePassesItemTests: XCTestCase {
    // MARK: - Test Cases for Backstage Passes Items
    func testBackstagePassesItems() {
        let items = [
            Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 15, quality: 20),
            Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 6, quality: 30),
            Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 7, quality: 28)]
        
        let app = GildedRose(items: items)
        
        let days = 6
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(9, app.items[0].sellIn)
        XCTAssertEqual(0, app.items[1].sellIn)
        XCTAssertEqual(1, app.items[2].sellIn)
        
        // quality value
        XCTAssertEqual(27, app.items[0].quality)
        XCTAssertEqual(47, app.items[1].quality)
        XCTAssertEqual(44, app.items[2].quality)
    }
    
    func testBackstagePassesItemAfterSellInDatePassed() {
        let items = [
            Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 5, quality: 49),
            Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 4, quality: 30)]
        
        let app = GildedRose(items: items)
        
        let days = 6
        for _ in 0..<days {
            app.updateQuality()
        }
        
        // sellIn value
        XCTAssertEqual(-1, app.items[0].sellIn)
        XCTAssertEqual(-2, app.items[1].sellIn)
        
        // quality value
        XCTAssertEqual(0, app.items[0].quality) // Quality of Backstage passes item is 0 after the sell in date is passed
        XCTAssertEqual(0, app.items[1].quality)
    }
    
    func testBackstagePassesItemsWithQualityMoreThanFifty() {
        let items = [ Item(name: "Backstage passes to a TAFKAL80ETC concert", sellIn: 10, quality: 49)]
        let app = GildedRose(items: items)
        let days = 3
        for _ in 0..<days {
            app.updateQuality()
        }
        // sellIn value
        XCTAssertEqual(7, app.items[0].sellIn)
        
        // quality value
        XCTAssertEqual(50, app.items[0].quality) // Quality of Backstage passes item cannot be more than 50
    }
    
}
