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

    old_item

    if name == 'Aged Brie'
      increase_item_quality
      increase_item_quality if sell_in < 0
    elsif name == 'Backstage passes to a TAFKAL80ETC concert'
      increase_item_quality if sell_in < 11
      increase_item_quality if sell_in < 6
      quality -= quality if sell_in < 0
    elsif name == 'Conjured Mana Cake'
      decrease_item_quality
      decrease_item_quality
      if sell_in < 0
        decrease_item_quality
        decrease_item_quality
      end
    else
      decrease_item_quality
      decrease_item_quality if sell_in < 0
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
