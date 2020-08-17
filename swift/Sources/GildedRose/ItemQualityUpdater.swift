//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

import Foundation

protocol ItemQualityUpdater {
    var item: Item {get set}
}

extension ItemQualityUpdater {
    var isItemUnderHighestQuality: Bool {
        return item.quality < ValueConstants.kHightestQualityValue
    }
    
    var isItemMoreThanLowestQuality: Bool {
        return item.quality > ValueConstants.kLowestQualityValue
    }
    
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
