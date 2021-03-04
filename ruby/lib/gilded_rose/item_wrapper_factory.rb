module GildedRose
  class ItemWrapperFactory

    SPECIAL_ITEM_WRAPPER_KLASSES = {
      "Aged Brie" => AgedBrieItemWrapper,
      "Backstage passes to a TAFKAL80ETC concert" => BackstagePassesItemWrapper,
      "Sulfuras, Hand of Ragnaros" => SulfurasItemWrapper,
    }

    GENERIC_ITEM_WRAPPER_KLASS = GenericItemWrapper

    def self.wrap(item: )
      klass = SPECIAL_ITEM_WRAPPER_KLASSES.fetch(item.name, GENERIC_ITEM_WRAPPER_KLASS)
      klass.new(item: item)
    end
  end
end

