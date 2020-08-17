//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct AgedBrieItem: ItemStateUpdater{
    var item: Item
        
    public init(item: Item) {
        self.item = item
    }
    
    func updateItemState() {
        // update the sell in days. Reduce the Sell In days by 1
        reduceSellInDays(by: 1)
        
        // Increment the Item quality by 1 if the quality is less than 50
        guard isItemUnderHighestQuality else { return }
        increaseItemQuality(by: 1)
    }
    
    
    
}
