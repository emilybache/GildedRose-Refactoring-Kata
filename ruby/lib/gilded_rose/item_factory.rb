module GildedRose
  class ItemFactory
    SPECIAL_ITEMS = ["Aged Brie", "Backstage passes to a TAFKAL80ETC concert", "Sulfuras, Hand of Ragnaros"]
    def self.create_item(name:, sell_in:, quality:)
      if SPECIAL_ITEMS.include?(name)
        return Item.new(name, sell_in, quality)
      end
  
      GenericItem.new(name, sell_in, quality)
    end
  end
end

