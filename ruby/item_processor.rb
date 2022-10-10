# frozen_string_literal: true

class ItemProcessor
  attr_accessor :item

  def initialize(item)
    @item = item
  end

  def update_item_quality
    return if never_sold

    if (item.name != 'Aged Brie') && (item.name != 'Backstage passes to a TAFKAL80ETC concert')
      decrease_item_quality
    else
      if item.quality < 50
        item.quality += 1
        if item.name == 'Backstage passes to a TAFKAL80ETC concert'
          increase_item_quality if item.sell_in < 11
          increase_item_quality if item.sell_in < 6
        end
      end
    end
    old_item
    if item.sell_in < 0
      if item.name != 'Aged Brie'
        if item.name != 'Backstage passes to a TAFKAL80ETC concert'
          decrease_item_quality
        else
          item.quality = item.quality - item.quality
        end
      else
        increase_item_quality
      end
    end
  end

  def increase_item_quality
    item.quality += 1 if item.quality < 50
  end

  def decrease_item_quality
    item.quality -= 1 if item.quality > 0 && item.name != 'Sulfuras, Hand of Ragnaros'
  end

  def old_item
    item.sell_in = item.sell_in - 1 if item.name != 'Sulfuras, Hand of Ragnaros'
  end

  def never_sold
    item.name == 'Sulfuras, Hand of Ragnaros'
  end
end
