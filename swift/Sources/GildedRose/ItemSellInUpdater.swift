//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

import Foundation

protocol ItemSellInUpdater: CustomisedItem{
    func reduceSellInDays(by days: Int)
}

extension ItemSellInUpdater {
    func reduceSellInDays(by days: Int) {
        item.sellIn -= days
    }
}
