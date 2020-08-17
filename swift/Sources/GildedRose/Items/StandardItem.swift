//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct StandardItem: ItemStateUpdater {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // Reduce the sellIn days for Item by 1
        reduceSellInDays(by: 1)

        // Reduce the item quality by 1 , if the sell in date is passed decrement by double the value
        isSellInDatePassed ? reduceItemQuality(by: decreasingValueForZeroOrLessDaysToSell()) : reduceItemQuality(by: decreasingValueOverZeroDaysToSell())
        
        guard isItemMoreThanLowestQuality else {
            // Sets the quality to zero if the quality is negative
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
