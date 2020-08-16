//
//  File.swift
//  
//
//  Created by Manali Mogre on 16/08/2020.
//

import Foundation

struct SulfurasItem: CustomisedItemProtocol {
    var item: Item
    
    public init(item: Item) {
        self.item = item
    }
    func updateCustomItemQuality() {
        // No code as there is no change in quality or sell in days of sulfuras item
    }
}
