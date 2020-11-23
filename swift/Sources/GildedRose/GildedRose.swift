public class GildedRose {
    private(set) var items: [Item]
    
    public init(items: [Item]) {
        self.items = items
    }
    
    public func updateQuality() {
        items = items.map { item in
            let qualityUpdatedItem = QualityModificationRule.process(item: item)
            return SellInModificationRule.process(item: qualityUpdatedItem)
        }
    }
}
