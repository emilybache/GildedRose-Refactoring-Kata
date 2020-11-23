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
         qualityIncreasesFasterBeforeExpireThenDropsToZero,
         legendary

    static func rule(for item: Item) -> RuleType {
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
        switch self {
        case .regular:
            return applyRegularRule(to: item)
        case .increasesQuality:
            return applyIncreasesQualityRule(to: item)
        case .qualityIncreasesFasterBeforeExpireThenDropsToZero:
            return qualityIncreasesFasterBeforeExpireThenDropsToZeroRule(to: item)
        case .legendary:
            return applyLegendaryRule(to: item)
        }
    }

    private func applyRegularRule(to item: Item) -> Item {
        let modification = Self.isRegularItemExpired(sellIn: item.sellIn) ? -2 : -1
        return Self.applyQualityThresholds(to: item, newQuality: item.quality + modification)
    }

    private func applyIncreasesQualityRule(to item: Item) -> Item {
        Self.applyQualityThresholds(to: item, newQuality: item.quality + 1)
    }

    private func qualityIncreasesFasterBeforeExpireThenDropsToZeroRule(to item: Item) -> Item {
        let newQuality: Int
        switch item.sellIn {
        case ...0:
            newQuality = 0
        case 0...5:
            newQuality = item.quality + 3
        case 5...10:
            newQuality = item.quality + 2
        case 10...:
            newQuality = item.quality + 1
        default:
            fatalError("All values of `sellIn` should be covered")
        }

        return Self.applyQualityThresholds(to: item, newQuality: newQuality)
    }

    private func applyLegendaryRule(to item: Item) -> Item {
        return item // No change
    }

    public static func applyQualityThresholds(to item: Item, newQuality: Int) -> Item {
        let correctedQuality: Int
        switch newQuality {
        case ...0:
            correctedQuality = 0
        case 0...50:
            correctedQuality = newQuality
        case 50...:
            correctedQuality = 50
        default:
            fatalError("All values of `quality` should be covered")
        }

        return Item(name: item.name, sellIn: item.sellIn, quality: correctedQuality)
    }

    public static func isRegularItemExpired(sellIn: Int) -> Bool {
        return sellIn <= 0
    }
}
