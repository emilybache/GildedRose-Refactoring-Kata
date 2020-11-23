//
//  QualityModificationRule.swift
//  
//
//  Created by Lucas van Dongen on 23/11/2020.
//

import Foundation

// These rules are really simple, you could handle more complex ones within their own class
enum QualityModificationRule: Rule {
    typealias RuleType = Self

    case regular,
         increasesQuality,
         conjured,
         qualityIncreasesFasterBeforeExpireThenDropsToZero,
         legendary

    static func rule(for item: Item) -> RuleType {
        if item.name.lowercased().contains("conjured") {
            return .conjured
        }

        switch item.name {
        case "Aged Brie":
            return .increasesQuality
        case "Backstage passes to a TAFKAL80ETC concert":
            return .qualityIncreasesFasterBeforeExpireThenDropsToZero
        case "Sulfuras, Hand of Ragnaros":
            return .legendary
        default:
            return .regular
        }
    }

    func apply(to item: Item) -> Item {
        let newQuality: Int
        switch self {
        case .regular:
            newQuality = regularRuleQuality(for: item)
        case .increasesQuality:
            newQuality = increasesQualityQuality(for: item)
        case .qualityIncreasesFasterBeforeExpireThenDropsToZero:
            newQuality = increasesFasterBeforeExpireThenDropsToZeroQuality(for: item)
        case .legendary:
            return item // Unmodified
        case .conjured:
            newQuality = conjuredRuleQuality(for: item)
        }

        return Self.modify(item: item, with: newQuality)
    }

    private func regularRuleQuality(for item: Item) -> Int {
        let modification = Self.isRegularItemExpired(sellIn: item.sellIn) ? -2 : -1
        return item.quality + modification
    }

    private func increasesQualityQuality(for item: Item) -> Int {
        return item.quality + 1
    }

    private func increasesFasterBeforeExpireThenDropsToZeroQuality(for item: Item) -> Int {
        switch item.sellIn {
        case ...0:
            return 0
        case 0...5:
            return item.quality + 3
        case 5...10:
            return item.quality + 2
        case 10...:
            return item.quality + 1
        default:
            fatalError("All values of `sellIn` should be covered")
        }
    }

    private func conjuredRuleQuality(for item: Item) -> Int {
        let modification = Self.isRegularItemExpired(sellIn: item.sellIn) ? -4 : -2
        return item.quality + modification
    }

    public static func modify(item: Item, with newQuality: Int) -> Item {
        let correctedQuality = applyQualityThresholds(to: newQuality)

        return Item(name: item.name, sellIn: item.sellIn, quality: correctedQuality)
    }

    public static func applyQualityThresholds(to quality: Int) -> Int {
        switch quality {
            case ...0:
                return 0
            case 0...50:
                return quality
            case 50...:
                return 50
            default:
                fatalError("All values of `quality` should be covered")
        }
    }

    public static func isRegularItemExpired(sellIn: Int) -> Bool {
        return sellIn <= 0
    }
}
