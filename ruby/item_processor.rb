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
    other_items
  end

  def other_items
    item.quality += quality_identifier
  end

  def quality_increase
    return 50 if quality > 50
    return 0 if item.quality < 0

    quality
  end

  def quality_identifier
    incrementor = 0
    if name == 'Aged Brie'
      incrementor += 1
      incrementor += 1 if sell_in < 0
    elsif name == 'Backstage passes to a TAFKAL80ETC concert'
      incrementor += 1 if sell_in < 11
      incrementor += 1 if sell_in < 6
      incrementor -= quality if sell_in < 0
    elsif name == 'Conjured Mana Cake'
      incrementor -= 2
      incrementor -= 2 if sell_in < 0
    else
      incrementor -= 1
      incrementor -= 1 if sell_in < 0
    end
    incrementor
  end

  def old_item
    item.sell_in -= 1
  end

  def never_sold
    name == 'Sulfuras, Hand of Ragnaros'
  end
end
