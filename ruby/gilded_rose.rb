class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each { |item| ItemUpdater.new(item) }
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

class ItemUpdater
  attr_accessor :item, :type, :quality_modifier

  def initialize(item)
    @item = item
    set_attributes
    update_quality
  end

  def set_attributes
    if /.*(^|\b)conjured(\b|$).*/i.match?(@item.name)
      @type = 'conjured'
      @quality_modifier = -2
    elsif /.*(^|\b)sulfuras(\b|$).*/i.match?(@item.name)
      @type = 'legendary'
    elsif /.*(^|\b)concert(\b|$).*/i.match?(@item.name)
      @type = 'concert_ticket'
      @quality_modifier = item.sell_in > 10 ? 1 : item.sell_in > 5 ? 2 : item.sell_in > 0 ? 3 : 1 - Float::INFINITY
    elsif /aged brie/i.match?(@item.name)
      @type = 'aged brie'
      @quality_modifier = 1
    else
      @type = 'basic'
      @quality_modifier = -1
    end
  end

  def update_quality
    return if @type == 'legendary'

    @item.sell_in = @item.sell_in - 1
    @item.quality = @item.sell_in < 0 ? @item.quality + (@quality_modifier * 2) : @item.quality + @quality_modifier
    @item.quality = 0 if @item.quality.negative?
    @item.quality = 50 if @item.quality > 50
  end
end
