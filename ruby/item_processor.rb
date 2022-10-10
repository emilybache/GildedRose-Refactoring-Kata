# frozen_string_literal: true

class ItemProcessor
  attr_accessor :item, :name, :sell_in, :quality

  def initialize(item)
    @item = item
    @quality = item.quality
    @sell_in = item.sell_in
    @name = item.name
  end

  def update_item_quality
    return if never_sold

    if (name != 'Aged Brie') && (name != 'Backstage passes to a TAFKAL80ETC concert')
      decrease_item_quality
    else
      if quality < 50
        quality += 1
        if name == 'Backstage passes to a TAFKAL80ETC concert'
          increase_item_quality if sell_in < 11
          increase_item_quality if sell_in < 6
        end
      end
    end
    old_item
    if sell_in < 0
      if name != 'Aged Brie'
        if name != 'Backstage passes to a TAFKAL80ETC concert'
          decrease_item_quality
        else
          quality -= quality
        end
      else
        increase_item_quality
      end
    end
  end

  def increase_item_quality
    quality += 1 if quality < 50
  end

  def decrease_item_quality
    quality -= 1 if item.quality > 0
  end

  def old_item
    item.sell_in -= 1
  end

  def never_sold
    name == 'Sulfuras, Hand of Ragnaros'
  end
end
