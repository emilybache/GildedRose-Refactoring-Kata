//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

protocol CustomisedItemProtocol {
    func updateCustomItemQuality()
}


protocol ItemSellInDaysProtocol {
    func reduceSellInDaysByOne(item: Item)
}

extension ItemSellInDaysProtocol {
    func reduceSellInDaysByOne(item: Item) {
        item.sellIn -= 1
    }
    
    func HasSellInDatePassed(item: Item) -> Bool {
        return item.sellIn < 0
    }
}

protocol ItemQualityProtocol {
    
}

extension ItemQualityProtocol {
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
