public class GildedRose {
    var items:[Item]
    
    public init(items:[Item]) {
        self.items = items
    }
    
    public func updateQuality() {
        for item in items {
            let customFactoryObj = CustomisedItemFactory()
            let customItem = customFactoryObj.getCustomisedItem(item: item)
            customItem.updateCustomItemQuality()
        }
    }
    
   /* public func updateQuality() {
        for item in items {
            switch item.name {
            case ItemNameConstants.kBackstagePassesItem:
                item.sellIn = item.sellIn - 1
                if item.sellIn < 0 {
                    item.quality = 0
                }
                else {
                    item.quality = item.quality < 50 ? item.quality + 1 : 50
                    if item.sellIn < 10 {
                        item.quality = item.quality < 50 ? item.quality + 1 : 50
                    }
                    if item.sellIn < 5 {
                        item.quality = item.quality < 50 ? item.quality + 1 : 50
                    }
                }
            case ItemNameConstants.kAgedBrieItem:
                item.sellIn = item.sellIn - 1
                item.quality = item.quality < 50 ? (item.quality + 1) : 50
            case ItemNameConstants.kSulfurasItem:
                break
            default:
                item.sellIn = item.sellIn - 1
                item.quality = item.quality > 0 ? (item.quality - 1) : 0
                if(item.sellIn < 0) {
                    item.quality = item.quality > 0 ? (item.quality - 1) : 0
                }
            }
        }
    }
    
    public func updateQuality() {
        for i in 0..<items.count {
            if (items[i].name != "Aged Brie" && items[i].name != "Backstage passes to a TAFKAL80ETC concert") {
                if (items[i].quality > 0) {
                    if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                        items[i].quality = items[i].quality - 1
                    }
                }
            } else {
                if (items[i].quality < 50) {
                    items[i].quality = items[i].quality + 1
                    
                    if (items[i].name == "Backstage passes to a TAFKAL80ETC concert") {
                        if (items[i].sellIn < 11) {
                            if (items[i].quality < 50) {
                                items[i].quality = items[i].quality + 1
                            }
                        }
                        
                        if (items[i].sellIn < 6) {
                            if (items[i].quality < 50) {
                                items[i].quality = items[i].quality + 1
                            }
                        }
                    }
                }
            }
            
            if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                items[i].sellIn = items[i].sellIn - 1
            }
            
            if (items[i].sellIn < 0) {
                if (items[i].name != "Aged Brie") {
                    if (items[i].name != "Backstage passes to a TAFKAL80ETC concert") {
                        if (items[i].quality > 0) {
                            if (items[i].name != "Sulfuras, Hand of Ragnaros") {
                                items[i].quality = items[i].quality - 1
                            }
                        }
                    } else {
                        items[i].quality = items[i].quality - items[i].quality
                    }
                } else {
                    if (items[i].quality < 50) {
                        items[i].quality = items[i].quality + 1
                    }
                }
            }
        }
    } */
}
