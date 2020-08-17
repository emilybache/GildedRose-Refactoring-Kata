//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

import Foundation

protocol ItemQualityUpdater: CustomisedItem {
}

extension ItemQualityUpdater {
   
    func reduceItemQuality(by value:Int) {
        item.quality -= value
    }
    
    func increaseItemQuality(by value:Int) {
        item.quality += value
    }
    
    func setItemQuality(to value: Int){
        item.quality = value
    }
}

typealias ItemStateUpdater = ItemSellInUpdater & ItemQualityUpdater
