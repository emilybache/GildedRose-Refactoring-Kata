//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct AgedBrieItem: CustomisedItem, ItemStateUpdater{
    
    var item: Item
    
    private var isItemUnderHighestQuality: Bool {
        // Returns true if quality of item is less than Hightest Quality(50)
        return item.quality < ValueConstants.kHightestQualityValue
    }
    
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // update the sell in days. Reduce the Sell In days by 1
        self.updateSellInDays()
        
        // Increment the Item quality by 1 if the quality is less than 50
        guard isItemUnderHighestQuality else { return }
        increaseItemQuality(by: 1)
    }
}
