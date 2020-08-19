//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

class StandardItem: CustomisedItem, ItemStateUpdater {
    var item: Item
    
    private var isSellInDatePassed: Bool{
        // Returns true if the sellin date is passed
        return item.sellIn < 0 
    }
    
    var decreasingQualityValueBeforeSellInDate: Int {
        return 1 // Quality degrades by 1 if the sellIn date is not passed
    }
    private var decreasingQualityValueAfterSellInDate: Int {
        return 2 * decreasingQualityValueBeforeSellInDate // Quality degrades twice after SellIn date is passed
    }
    
    private  var isItemMoreThanLowestQuality: Bool {
        // Returns true if Quality of item is more than lowest quality(0)
        return item.quality > ValueConstants.kLowestQualityValue
    }
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // Reduce the sellIn days for Item by 1
        updateSellInDays()
        
        // Reduce the item quality by 1 , if the sell in date is passed decrement by double the value
        isSellInDatePassed ? reduceItemQuality(by: decreasingQualityValueAfterSellInDate) : reduceItemQuality(by: decreasingQualityValueBeforeSellInDate)
        
        guard isItemMoreThanLowestQuality else {
            // Sets the quality to zero if the quality is negative
            setItemQuality(to: ValueConstants.kLowestQualityValue)
            return
        }
    }
}
