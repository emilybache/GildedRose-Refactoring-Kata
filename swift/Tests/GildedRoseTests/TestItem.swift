//
//  TestItem.swift
//  
//
//  Created by Lucas van Dongen on 23/11/2020.
//

@testable import GildedRose
import Foundation

extension Item {
    func updated(hasExpectedQuality quality: Int) -> Bool {
        let system = GildedRose(items: [self])
        system.updateQuality()

        return system.items.first?.quality == quality
    }
}
