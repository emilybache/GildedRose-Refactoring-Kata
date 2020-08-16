//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

protocol CustomisedItemProtocol {
    func updateItemState()
}


protocol ItemSellInUpdater {
    func reduceSellInDays(for item: Item, by days: Int)
}

extension ItemSellInUpdater {
    func reduceSellInDays(for item: Item, by days: Int) {
        item.sellIn -= days
    }
    
    func HasSellInDatePassed(item: Item) -> Bool {
        return item.sellIn < 0
    }
}

protocol ItemQualityUpdater {
    
}

extension ItemQualityUpdater {
    func isItemUnderHighestQuality(item: Item) -> Bool {
        return item.quality < ValueConstants.kHightestQualityValue
    }
    func reduceQuality(for item: Item, by value:Int) {
        item.quality -= value
    }
    
    func increaseQuality(for item: Item, by value:Int) {
        item.quality += value
    }
    
    func isItemOverLowestQuality(item: Item) -> Bool {
        return item.quality > ValueConstants.kLowestQualityValue
    }
    
    func setItemQualityToZero(item: Item) {
        item.quality = ValueConstants.kLowestQualityValue
    }
    
    func setItemQualityToFifty(item: Item) {
        item.quality = ValueConstants.kHightestQualityValue
    }
}
