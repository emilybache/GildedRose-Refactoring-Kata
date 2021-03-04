module GildedRose
  class ItemFactory
    SPECIAL_ITEM_KLASSES = {
      "Aged Brie" => AgedBrieItem,
      "Backstage passes to a TAFKAL80ETC concert" => BackstagePassesItem,
      "Sulfuras, Hand of Ragnaros" => SulfurasItem,
    }

    GENERIC_ITEM_KLASS = GenericItem

    def self.create_item(name:, sell_in:, quality:)
      klass = SPECIAL_ITEM_KLASSES.fetch(name, GENERIC_ITEM_KLASS)
      klass.new(name, sell_in, quality)
    end
  end
end

