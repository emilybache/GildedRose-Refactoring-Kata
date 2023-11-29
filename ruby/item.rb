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

  def is_aged_brie?
    name == "Aged Brie"
  end

  def is_backstage_passes?
    name == "Backstage passes to a TAFKAL80ETC concert"
  end

  def is_sulfuras?
    name == "Sulfuras, Hand of Ragnaros"
  end

  def is_conjured_mana_cake?
    name == "Conjured Mana Cake"
  end
end
