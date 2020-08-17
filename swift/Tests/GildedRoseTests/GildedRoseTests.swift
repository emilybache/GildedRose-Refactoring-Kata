@testable import GildedRose
import XCTest

class GildedRoseTests: XCTestCase {

    func testFoo() {
        let items = [Item(name: "foo", sellIn: 0, quality: 0)]
        let app = GildedRose(items: items);
        app.updateQuality();
        XCTAssertEqual("foo", app.items[0].name);
    }

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
    
    static var allTests : [(String, (GildedRoseTests) -> () throws -> Void)] {
        return [
            ("testFoo", testFoo),
        ]
    }
}
