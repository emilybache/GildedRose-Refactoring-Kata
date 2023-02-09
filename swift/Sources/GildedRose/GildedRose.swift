import Foundation

public class GildedRose {
    private static let agedBrieString = "Aged Brie"
    private static let backstagePassString = "Backstage passes to a TAFKAL80ETC concert"
    private static let sulfurasString = "Sulfuras, Hand of Ragnaros"
    private static let conjuredString = "conjured"
    
    private static let minimumQuality = 0
    private static let maximumQuality = 50
    
    var items: [Item]

    public init(items: [Item]) {
        self.items = items
    }
    
    private func calculateDegradeRate(_ item: Item, isExpired: Bool) -> Int {
        let degradeRate = item.name.lowercased().range(of: GildedRose.conjuredString) != nil ? -2 : -1
        return isExpired ? degradeRate * 2 : degradeRate
    }
    
    private func adjustQuality(_ item: Item, adjustment: Int) {
        let adjustedQuality = item.quality + adjustment
        if adjustedQuality <= GildedRose.maximumQuality && adjustedQuality >= GildedRose.minimumQuality {
            item.quality += adjustment
        }
    }
    
    private func adjustBackstagePassQuality(_ item: Item, isExpired: Bool) {
        self.adjustQuality(item, adjustment: 1)
        if item.sellIn < 11 {
            self.adjustQuality(item, adjustment: 1)
        }
        if item.sellIn < 6 {
            self.adjustQuality(item, adjustment: 1)
        }
        if isExpired {
            self.adjustQuality(item, adjustment: -item.quality)
        }
    }

    private func updateItemQuality(_ item: Item) {
        let isExpired = item.sellIn < 1
        let itemQualityDegradationAmount = self.calculateDegradeRate(item, isExpired: isExpired)
        if item.name != GildedRose.agedBrieString && item.name != GildedRose.backstagePassString && item.name != GildedRose.sulfurasString {
            self.adjustQuality(item, adjustment: itemQualityDegradationAmount)
        }
        if item.name == GildedRose.agedBrieString {
            self.adjustQuality(item, adjustment: isExpired ? 2 : 1)
        }
        if item.name == GildedRose.backstagePassString {
            self.adjustBackstagePassQuality(item, isExpired: isExpired)
        }
        if item.name != GildedRose.sulfurasString {
            item.sellIn = item.sellIn - 1
        }
    }
    
    public func updateQuality() {
        items.forEach({ item in
            updateItemQuality(item)
        })
    }
}
