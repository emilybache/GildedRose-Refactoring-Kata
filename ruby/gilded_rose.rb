class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      # Skip updating 'Sulfuras' items, as they have fixed quality and do not degrade
      next if item.name == 'Sulfuras, Hand of Ragnaros'

      # Update item quality based on item type
      item.quality = case item.name
                     when 'Aged Brie'
                       [50, item.quality + update_count(item)].min
                     when 'Backstage passes to a TAFKAL80ETC concert'
                       backstage_passes(item)
                     when 'Conjured Mana Cake'
                       [0, item.quality - update_count(item) * 2].max
                     else
                       [0, item.quality - update_count(item)].max
                     end

      item.sell_in -= 1
    end
  end

  private

  # Helper method to determine the update count based on sell_in
  def update_count(item)
    (item.sell_in.negative? ? 2 : 1)
  end

  # Helper method to handle 'Backstage passes' quality update
  def backstage_passes(item)
    return 0 if item.sell_in.negative?

    if item.sell_in <= 5
      [50, item.quality + 3]
    elsif item.sell_in <= 10
      [50, item.quality + 2]
    else
      [50, item.quality + 1]
    end.min
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
