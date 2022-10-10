# frozen_string_literal: true

class ItemProcessor
  attr_accessor :item

  def initialize(item)
    @item = item
  end

  def update_item_quality
    if (item.name != 'Aged Brie') && (item.name != 'Backstage passes to a TAFKAL80ETC concert')
      if item.quality > 0
        item.quality -= 1 if item.name != 'Sulfuras, Hand of Ragnaros'
      end
    else
      if item.quality < 50
        item.quality += 1
        if item.name == 'Backstage passes to a TAFKAL80ETC concert'
          if item.sell_in < 11
            item.quality += 1 if item.quality < 50
          end
          if item.sell_in < 6
            item.quality += 1 if item.quality < 50
          end
        end
      end
    end
    item.sell_in = item.sell_in - 1 if item.name != 'Sulfuras, Hand of Ragnaros'
    if item.sell_in < 0
      if item.name != 'Aged Brie'
        if item.name != 'Backstage passes to a TAFKAL80ETC concert'
          if item.quality > 0
            item.quality -= 1 if item.name != 'Sulfuras, Hand of Ragnaros'
          end
        else
          item.quality = item.quality - item.quality
        end
      else
        item.quality += 1 if item.quality < 50
      end
    end
  end
end
