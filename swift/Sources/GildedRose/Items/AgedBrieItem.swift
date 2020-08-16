//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct AgedBrieItem: CustomisedItemProtocol, ItemSellInUpdater, ItemQualityUpdater {
    var item: Item
   
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // update the sell in days
        reduceSellInDays(for: item, by: 1)
        
        // Increment the Item quality by 1 if the quality is less than 50
        guard isItemUnderHighestQuality(item: item) else { return }
        increaseQuality(for: item, by: 1)
    }

}
