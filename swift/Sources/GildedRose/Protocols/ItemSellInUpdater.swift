//
//  File.swift
//  
//
//  Created by Manali Mogre on 17/08/2020.
//

import Foundation

protocol ItemSellInUpdater: CustomisedItem{
    var decreasingNumberOfSellInDays: Int { get }
    func updateSellInDays()
}

extension ItemSellInUpdater {
    var decreasingNumberOfSellInDays: Int {
           return 1
    }
    
    // Reduces the sell in days by 1
    func updateSellInDays() {
        item.sellIn -= decreasingNumberOfSellInDays
    }
}
