class GildedRose
  def self.update_quality(items)
    items.each do |item|
      update_item_quality(item)
    end
  end

  private

  def self.update_item_quality(item)
    case item.name
    when "Aged Brie"
      update_aged_brie(item)
    when "Backstage passes to a TAFKAL80ETC concert"
      update_backstage_passes(item)
    when "Sulfuras, Hand of Ragnaros"
      # Sulfuras does not change, nothing to do
    when "Conjured"
      update_conjured(item)
    else
      update_normal_item(item)
    end
  end

  def self.update_aged_brie(item)
    increase_quality(item)
    decrease_sell_in(item)
    increase_quality(item) if item.sell_in.negative?
  end

  def self.update_backstage_passes(item)
    increase_quality(item)
    decrease_sell_in(item)

    if item.sell_in < 10
      increase_quality(item)
    end

    if item.sell_in < 5
      increase_quality(item)
    end

    item.quality = 0 if item.sell_in.negative?
  end

  def self.update_conjured(item)
    decrease_quality(item, 2)
    decrease_sell_in(item)
  end

  def self.update_normal_item(item)
    decrease_quality(item)
    decrease_sell_in(item)
    decrease_quality(item, 1) if item.sell_in.negative?
  end

  def self.decrease_quality(item, amount = 1)
    item.quality -= amount if item.quality.positive?
  end

  def self.increase_quality(item)
    item.quality += 1 if item.quality < 50
  end

  def self.decrease_sell_in(item)
    item.sell_in -= 1 unless item.name == "Sulfuras, Hand of Ragnaros"
    item.sell_in = 0 if item.name == "Aged Brie" && item.sell_in.negative? 
  end
end

class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name, sell_in, quality)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end

  def to_s()
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
