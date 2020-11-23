//
//  SellInModificationRule.swift
//  
//
//  Created by Lucas van Dongen on 23/11/2020.
//

import Foundation

enum SellInModificationRule: Rule {
    typealias RuleType = Self

    case regular, legendary

    static func rule(for item: Item) -> RuleType {
        switch item.name {
        case "Sulfuras, Hand of Ragnaros":
            return .legendary
        default:
            return .regular
        }
    }

    func apply(to item: Item) -> Item {
        switch self {
        case .regular:
            return applyRegularRule(to: item)
        case .legendary:
            return item // Unmodified
        }
    }

    func applyRegularRule(to item: Item) -> Item {
        return Item(name: item.name, sellIn: item.sellIn - 1, quality: item.quality)
    }
}
