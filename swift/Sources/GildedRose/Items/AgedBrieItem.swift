//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct AgedBrieItem: CustomisedItemProtocol, ItemSellInDaysProtocol, ItemQualityProtocol {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    func updateCustomItemQuality() {
        // update the sell in days
        reduceSellInDaysByOne(item: item)
        // Increment the Item quality by 1 if the quality is less than 50
        guard isItemUnderHighestQuality(item: item) else {return}
        increaseQuality(for: item, by: 1)
    }

}
