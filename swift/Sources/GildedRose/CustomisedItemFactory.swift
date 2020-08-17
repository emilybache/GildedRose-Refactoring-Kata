//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

protocol CustomisedItem {
    var item: Item {get set}
    func updateItemState()
}

class CustomisedItemFactory {
    // Returns the Customised Item based on the Item name
     func getCustomisedItem(item: Item) -> CustomisedItem {
        switch item.name {
        case ItemNameConstants.kAgedBrieItem:
            return AgedBrieItem(item: item)
        case ItemNameConstants.kBackstagePassesItem:
            return BackstagePassesItem(item: item)
        case ItemNameConstants.kSulfurasItem:
            return SulfurasItem(item: item)
        default:
            return StandardItem(item: item)
        }
    }
}




