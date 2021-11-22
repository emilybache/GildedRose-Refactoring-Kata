@testable import GildedRose
import XCTest

class GildedRoseTests: XCTestCase {

    func testFoo() throws {
        let items = [Item(name: "foo", sellIn: 0, quality: 0)]
        let app = GildedRose(items: items)
        app.updateQuality()
        XCTAssertEqual(app.items[0].name, "fixme")
    }
}
