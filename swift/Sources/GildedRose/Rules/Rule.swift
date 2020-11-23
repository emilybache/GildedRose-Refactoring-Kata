//
//  File.swift
//  
//
//  Created by Lucas van Dongen on 23/11/2020.
//

import Foundation

protocol Rule {
    associatedtype RuleType: Rule

    func apply(to item: Item) -> Item

    static func rule(for item: Item) -> RuleType
    static func process(item: Item) -> Item
}

extension Rule {
    static func process(item: Item) -> Item {
        return rule(for: item).apply(to: item)
    }
}
