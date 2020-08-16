public class GildedRose {
    var items:[Item]
    
    public init(items:[Item]) {
        self.items = items
    }
    
    public func updateQuality() {
        let customFactoryObj = CustomisedItemFactory()
       _ = items.map({customFactoryObj.getCustomisedItem(item: $0).updateItemState()})
     
    }
}
