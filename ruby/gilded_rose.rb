class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      update_item(item)
    end
  end

  private

  def update_item(item)
    if item.name == 'Sulfuras, Hand of Ragnaros'
      return # Legendary item, no need to update
    end

    update_sell_in(item)

    case item.name
    when 'Aged Brie'
      update_aged_brie(item)
    when 'Backstage passes to a TAFKAL80ETC concert'
      update_backstage_pass(item)
    when 'Conjured Mana Cake'
      update_conjured_item(item)
    else
      update_normal_item(item)
    end
  end

  def update_sell_in(item)
    item.sell_in -= 1 unless item.name == 'Sulfuras, Hand of Ragnaros'
  end

  def update_aged_brie(item)
    increase_quality(item)
    increase_quality(item) if item.sell_in < 0
  end

  def update_backstage_pass(item)
    if item.sell_in <= 0
      item.quality = 0
    elsif item.sell_in <= 5
      increase_quality(item, 3)
    elsif item.sell_in <= 10
      increase_quality(item, 2)
    else
      increase_quality(item)
    end
  end

  def update_conjured_item(item)
    decrease_quality(item, 2)
    decrease_quality(item, 2) if item.sell_in < 0
  end

  def update_normal_item(item)
    decrease_quality(item)
    decrease_quality(item) if item.sell_in < 0
  end

  def increase_quality(item, amount = 1)
    item.quality = [item.quality + amount, 50].min
  end

  def decrease_quality(item, amount = 1)
    item.quality = [item.quality - amount, 0].max
  end
end

class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name, sell_in, quality)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end
end
