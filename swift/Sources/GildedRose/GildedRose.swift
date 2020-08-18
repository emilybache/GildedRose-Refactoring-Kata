public class GildedRose {
    var items:[Item]
    
    public init(items:[Item]) {
        self.items = items
    }
    
    public func updateQuality() {
        let itemFactoryManager = CustomItemFactoryManager(customItemFactory: CustomisedItemFactoryWithNewItems())
       _ = items.map({ itemFactoryManager.getCustomisedItem(item: $0).updateItemState()})
    }
}
