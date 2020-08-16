//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct BackstagePassesItem: CustomisedItemProtocol, ItemQualityProtocol, ItemSellInDaysProtocol {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    func updateCustomItemQuality() {
        reduceSellInDaysByOne(item: item)
        
        if isSellInDaysMoreThanOrEqualTo(days: 10) {
            increaseQuality(for: item, by: 1)
        }
        else if isSellInDaysMoreThanOrEqualTo(days: 5) {
            increaseQuality(for: item, by: 2)
        }
        else if isSellInDaysMoreThanOrEqualTo(days: 0) {
            increaseQuality(for: item, by: 3)
        }
        if HasSellInDatePassed(item: item) {
            item.quality = ValueConstants.kLowestQualityValue
        }
        if !isItemUnderHighestQuality(item: item) {
            item.quality = ValueConstants.kHightestQualityValue
        }
    }
    
    private func isSellInDaysMoreThanOrEqualTo(days: Int) -> Bool {
           return item.sellIn >= days
       }
       
}
