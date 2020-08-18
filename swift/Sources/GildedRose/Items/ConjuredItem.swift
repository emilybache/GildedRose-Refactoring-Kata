//
//  File.swift
//  
//
//  Created by Manali Mogre on 18/08/2020.
//

import Foundation

class ConjuredItem: StandardItem{
    
    override var decreasingValueOverZeroDaysToSell: Int {
        return 2
    }
    override init(item: Item) {
        super.init(item: item)
    }
}
