import Foundation

public class GildedRose {
    private static let agedBrieString = "Aged Brie"
    private static let backstagePassString = "Backstage passes to a TAFKAL80ETC concert"
    private static let sulfurasString = "Sulfuras, Hand of Ragnaros"
    
    private static let itemQualityDegradationAmount = -1
    private static let minimumQuality = 0
    private static let maximumQuality = 50
    
    var items: [Item]

    public init(items: [Item]) {
        self.items = items
    }
    
    private func adjustQuality(_ item: Item, adjustment: Int) {
        let adjustedQuality = item.quality + adjustment
        if adjustedQuality <= GildedRose.maximumQuality && adjustedQuality >= GildedRose.minimumQuality {
            item.quality += adjustment
        }
    }

    private func updateItemQuality(_ item: Item) {
        if item.name != GildedRose.agedBrieString, item.name != GildedRose.backstagePassString {
            if item.name != GildedRose.sulfurasString {
                self.adjustQuality(item, adjustment: item.name.lowercased().range(of: "conjured") != nil ? GildedRose.itemQualityDegradationAmount * 2 : GildedRose.itemQualityDegradationAmount)
            }
        } else {
            self.adjustQuality(item, adjustment: 1)
            
            if item.name == GildedRose.backstagePassString {
                if item.sellIn < 11 {
                    self.adjustQuality(item, adjustment: 1)
                }
                
                if item.sellIn < 6 {
                    self.adjustQuality(item, adjustment: 1)
                }
            }
        }
        if item.name != GildedRose.sulfurasString {
            item.sellIn = item.sellIn - 1
        }
        if item.sellIn < 0 {
            if item.name != GildedRose.agedBrieString {
                if item.name != GildedRose.backstagePassString {
                    if item.name != GildedRose.sulfurasString {
                        self.adjustQuality(item, adjustment: item.name.lowercased().range(of: "conjured") != nil ? GildedRose.itemQualityDegradationAmount * 2 : GildedRose.itemQualityDegradationAmount)
                    }
                } else {
                    self.adjustQuality(item, adjustment: -item.quality)
                }
            } else {
                self.adjustQuality(item, adjustment: 1)
            }
        }
    }
    
    public func updateQuality() {
        items.forEach({ item in
            updateItemQuality(item)
        })
    }
}
