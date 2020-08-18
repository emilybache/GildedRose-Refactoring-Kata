//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

import Foundation

protocol ItemQualityUpdater: CustomisedItem {
    func reduceItemQuality(by value:Int)
    func increaseItemQuality(by value:Int)
    func setItemQuality(to value: Int)
}

extension ItemQualityUpdater {
   // Reduces the item Quality by the value passed as parameter
    func reduceItemQuality(by value:Int) {
        item.quality -= value
    }
    
    // Increases Item Quality by the value passed as parameter
    func increaseItemQuality(by value:Int) {
        item.quality += value
    }
    
    // Sets the Item Quality to the value passed as parameter
    func setItemQuality(to value: Int){
        item.quality = value
    }
}

typealias ItemStateUpdater = ItemSellInUpdater & ItemQualityUpdater
