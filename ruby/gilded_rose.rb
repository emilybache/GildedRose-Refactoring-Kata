class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      next if item.name == 'Sulfuras, Hand of Ragnaros'

      item.sell_in -= 1

      quality_change = case item.name
      when 'Backstage passes to a TAFKAL80ETC concert'
        if item.sell_in.negative?
          item.quality = 0
          next
        end

        if item.sell_in >= 10
          1
        elsif item.sell_in >= 5
          2
        else
          3
        end
      when 'Conjured Mana Cake'
        item.sell_in < 0 ? 4 : 2
      else
        quality_degradation = item.sell_in < 0 ? 2 : 1

        quality_degradation *= 2 if item.name.start_with?('Conjured')

        quality_degradation
      end

      new_quality = if [
        'Aged Brie',
        'Backstage passes to a TAFKAL80ETC concert',
      ].include?(item.name)
        item.quality + quality_change
      else
        item.quality - quality_change
      end

      item.quality = [[new_quality, 50].min, 0].max
    end
  end
end

class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name:, sell_in:, quality:)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end

  def to_s()
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
