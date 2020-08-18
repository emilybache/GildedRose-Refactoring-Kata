//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

protocol CustomisedItemFactoryCreator {
    func getCustomisedItem(item: Item) -> CustomisedItem
}

class CustomisedItemFactory: CustomisedItemFactoryCreator {
    // Returns the Created Customised Item based on the Item name
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

class CustomisedItemFactoryWithNewItems: CustomisedItemFactory {
    // Creates Conjured Item for newly added Conjured Item. For the old items calls the super class function
    override func getCustomisedItem(item: Item) -> CustomisedItem {
        switch item.name {
        case ItemNameConstants.kConjuredItem:
            return ConjuredItem(item: item)
        default:
           return super.getCustomisedItem(item: item)
        }
    }
}

final class CustomItemFactoryManager {
    var itemFactory: CustomisedItemFactoryCreator
    public init(customItemFactory: CustomisedItemFactoryCreator) {
        self.itemFactory = customItemFactory
    }
     func getCustomisedItem(item: Item) -> CustomisedItem {
        return itemFactory.getCustomisedItem(item: item)
    }
}
