//
//  File.swift
//  
//
//  Created by Manali Mogre on 18/08/2020.
//

import Foundation

class ConjuredItem: StandardItem{
   // Overrides decreasingQualityValueBeforeSellInDate property of Standard item as the Conjured Item degrades twice fast than Standard Iten
    override var decreasingQualityValueBeforeSellInDate: Int {
        return 2
    }
    override init(item: Item) {
        super.init(item: item)
    }
}
