class GildedRose
  REGULAR_MAX_QUALITY = 50
  REGULAR_MIN_QUALITY = 0
  INVENTORY_CLASSIFICATION = {
    nil => Proc.new do |sell_in:, quality:|
      next_sell_in = sell_in -1
      {
        sell_in: next_sell_in,
        quality: self.regular_quality_limit(
          quality + (next_sell_in < 0 ? -2 : -1)
        )
      }
    end,
    'Aged Brie' => Proc.new do |sell_in:, quality:|
      next_sell_in = sell_in -1
      {
        sell_in: next_sell_in,
        quality: self.regular_quality_limit(
          quality + (next_sell_in < 0 ? 2 : 1)
        )
      }
    end,
    'Sulfuras, Hand of Ragnaros' => Proc.new do |sell_in:, quality:|
      {
        sell_in: sell_in,
        quality: 80
      }
    end,
    'Backstage passes to a TAFKAL80ETC concert' => Proc.new do |sell_in:, quality:|
      next_sell_in = sell_in -1
      next_quality = quality + 1
      next_quality += 1 if next_sell_in < 10
      next_quality += 1 if next_sell_in < 5
      next_quality = 0 if next_sell_in < 0
      {
        sell_in: next_sell_in,
        quality: self.regular_quality_limit(next_quality)
      }
    end,
    "Conjured Mana Cake" => Proc.new do |sell_in:, quality:|
      next_sell_in = sell_in -1
      {
        sell_in: next_sell_in,
        quality: self.regular_quality_limit(
          quality + (next_sell_in < 0 ? -4 : -2)
        )
      }
    end,
  }

  def self.regular_quality_limit(unsanitized_quality)
    [
      [REGULAR_MIN_QUALITY, unsanitized_quality].max,
      REGULAR_MAX_QUALITY
    ].min
  end

  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each do |item|
      new_values = (INVENTORY_CLASSIFICATION[item.name] || INVENTORY_CLASSIFICATION[nil]).call(
        sell_in: item.sell_in,
        quality: item.quality
      )
      item.sell_in = new_values[:sell_in]
      item.quality = new_values[:quality]
    end
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
