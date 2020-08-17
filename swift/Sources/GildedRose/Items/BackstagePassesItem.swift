//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct BackstagePassesItem: CustomisedItem, ItemQualityUpdater, ItemSellInUpdater {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        reduceSellInDays(by: 1)
        
        guard !isSellInDatePassed else {
            setItemQuality(to: ValueConstants.kLowestQualityValue)
            return
        }
        guard isItemUnderHighestQuality else {
            setItemQuality(to: ValueConstants.kHightestQualityValue)
            return
        }
        switch item.sellIn {
        case 10...:
            increaseItemQuality(by: 1)
        case 5..<10:
            increaseItemQuality(by: 2)
        case 0..<5:
            increaseItemQuality(by: 3)
        default:
            break
        }
    }
}
