//
//  ConjuredRules.swift
//  GildedRose
//
//  Created by Lucas van Dongen on 23/11/2020.
//

@testable import GildedRose
import XCTest

class ConjuredRules: XCTestCase {
    private let manaCakeItem = Item(name: "Conjured Mana Cake", sellIn: 3, quality: 6)
    private let differentConjuredItem = Item(name: "Conjured Different Name", sellIn: 3, quality: 6)
    private let conjuredDifferentPlaceInNameItem = Item(name: "This item has been conjured", sellIn: 3, quality: 6)
    private let notAConjuredItem = Item(name: "Regular Item", sellIn: 3, quality: 6)

    //- "Conjured" items degrade in Quality twice as fast as normal items
    func testConjuredDegradesTwiceAsFast() {
        let initialQuality = manaCakeItem.quality
        let system = GildedRose(items: [manaCakeItem])

        system.updateQuality()
        XCTAssertEqual(system.items.first!.quality, initialQuality - 2)
    }

    //- All items with "Conjured" in their name are considered conjured
    func testAnythingConjured() {
        XCTAssertTrue(differentConjuredItem.updated(hasExpectedQuality: 4))
        XCTAssertTrue(conjuredDifferentPlaceInNameItem.updated(hasExpectedQuality: 4))
        XCTAssertTrue(manaCakeItem.updated(hasExpectedQuality: 4))
    }

    //- All items that do not have "Conjured" in their name are not considered conjured
    func testNonConjured() {
        XCTAssertTrue(notAConjuredItem.updated(hasExpectedQuality: 5))
    }
}
