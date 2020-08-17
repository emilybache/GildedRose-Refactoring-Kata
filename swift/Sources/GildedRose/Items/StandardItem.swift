//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct StandardItem: CustomisedItem, ItemQualityUpdater, ItemSellInUpdater {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // Reduce the sellIn days for Item by 1
        reduceSellInDays(by: 1)

        // Reduce the item quality by 1 , if the sell in date is passed decrement by 2
        isSellInDatePassed ? reduceItemQuality(by: decreasingValueForZeroOrLessDaysToSell()) : reduceItemQuality(by: decreasingValueOverZeroDaysToSell())
        guard isItemMoreThanLowestQuality else {
            setItemQuality(to: ValueConstants.kLowestQualityValue)
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
