//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct BackstagePassesItem: CustomisedItem, ItemStateUpdater {
    
    var item: Item
    
    private var isItemUnderHighestQuality: Bool {
        // Returns true if Item Quality is less than Highest value (50)
        return item.quality < ValueConstants.kHightestQualityValue
    }
    
    private var isSellInDatePassed: Bool{
        // Returns true if sell in date is passed
        return item.sellIn < 0
    }
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // Reduce the Sell in days by 1
        updateSellInDays()
        
        // If the sell in date is passed, sets the quality of item to 0
        guard !isSellInDatePassed else {
            setItemQuality(to: ValueConstants.kLowestQualityValue)
            return
        }
        // If the Quality of item is above 50, return
        guard isItemUnderHighestQuality else {
            setItemQuality(to: ValueConstants.kHightestQualityValue)
            return
        }
        switch item.sellIn {
        case 10...:
            increaseItemQuality(by: 1) // If the sell in days is more than 10, increase quality by 1
        case 5..<10:
            increaseItemQuality(by: 2) // If sell in days is between 5 and 10, increase quality by 2
        case 0..<5:
            increaseItemQuality(by: 3) // If sell in days is between 0 and 5, increase quality by 3
        default:
            break
        }
    }
}
