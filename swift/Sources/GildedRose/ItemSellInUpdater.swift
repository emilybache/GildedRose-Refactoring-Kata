//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

import Foundation

protocol ItemSellInUpdater {
    var item: Item { get set }
    func reduceSellInDays(by days: Int)
}

extension ItemSellInUpdater {
    var isSellInDatePassed: Bool{
        return item.sellIn < 0
    }
    func reduceSellInDays(by days: Int) {
        item.sellIn -= days
    }
}
