//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct BackstagePassesItem: CustomisedItemProtocol, ItemQualityUpdater, ItemSellInUpdater {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        reduceSellInDays(for: item, by: 1)

        guard !HasSellInDatePassed(item: item) else {
            setItemQualityToZero(item: item)
            return
        }
        guard isItemUnderHighestQuality(item: item) else {
            setItemQualityToFifty(item: item)
            return
        }
        switch item.sellIn {
        case 10...:
            increaseQuality(for: item, by: 1)
        case 5..<10:
            increaseQuality(for: item, by: 2)
        case 0..<5:
            increaseQuality(for: item, by: 3)
        default:
            break
        }
    }
       
}
