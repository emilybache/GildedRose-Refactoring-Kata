public class GildedRose {
    var items:[Item]
    
    public init(items:[Item]) {
        self.items = items
    }
    
    public func updateQuality() {
       _ = items.map({CustomisedItemFactory.getCustomisedItem(item: $0).updateItemState()})
    }
}
