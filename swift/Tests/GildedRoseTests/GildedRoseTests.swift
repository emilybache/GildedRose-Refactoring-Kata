@testable import GildedRose
import XCTest

class GildedRoseTests: XCTestCase {

    func testFoo() {
        let items = [Item(name: "foo", sellIn: 0, quality: 0)]
        let app = GildedRose(items: items);
        app.updateQuality();
        XCTAssertEqual("fixme", app.items[0].name);
    }

    static var allTests : [(String, (GildedRoseTests) -> () throws -> Void)] {
        return [
            ("testFoo", testFoo),
        ]
    }
}
