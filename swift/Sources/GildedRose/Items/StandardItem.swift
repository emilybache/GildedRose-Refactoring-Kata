//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct StandardItem: CustomisedItemProtocol, ItemQualityUpdater, ItemSellInUpdater {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    func updateItemState() {
        // Reduce the sellIn days for Item by 1
        reduceSellInDays(for: item, by: 1)

        // Reduce the item quality by 1 , if the sell in date is passed decrement by 2
        HasSellInDatePassed(item: item) ? reduceQuality(for: item, by: decreasingValueForZeroOrLessDaysToSell()) : reduceQuality(for: item, by: decreasingValueOverZeroDaysToSell())
        guard isItemOverLowestQuality(item: item) else {
            item.quality = ValueConstants.kLowestQualityValue
            return
        }
    }
    
    func decreasingValueOverZeroDaysToSell() -> Int {
        return 1
    }
    
    private func decreasingValueForZeroOrLessDaysToSell() -> Int {
        return 2 * decreasingValueOverZeroDaysToSell()
    }
}
