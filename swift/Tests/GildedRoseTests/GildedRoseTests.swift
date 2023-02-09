@testable import GildedRose
import XCTest

class GildedRoseTests: XCTestCase {
    func test_updateQuality_itemSellIn_shouldDecreaseAfterCallingFunction() throws {
        let items = [Item(name: "foo", sellIn: 5, quality: 5)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 4)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 3)
    }
    
    func test_updateQuality_itemQuality_shouldDegradeTwiceAsFastAfterPassingSellByDate() throws {
        let items = [Item(name: "foo", sellIn: 0, quality: 5)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 3)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 1)
    }
    
    func test_updateQuality_itemQuality_shouldntBecomeNegativeAfterPassingSellByDate() throws {
        let items = [Item(name: "foo", sellIn: 0, quality: 0)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 0)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 0)
    }
    
    func test_updateQuality_itemQuality_shouldntGoAbove50() throws {
        let items = [Item(name: "Aged Brie", sellIn: 1, quality: 49)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 50)
        app.updateQuality()
        XCTAssertEqual(app.items[0].quality, 50)
    }
}
