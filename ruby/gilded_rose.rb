class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each { |item| update_item(item) }
  end

  private

  def update_item(item)
    validate_item(item)
    case item.name
    when "Aged Brie"
      update_aged_brie(item)
    when "Sulfuras, Hand of Ragnaros"
      item.quality = 80 # Ensure quality remains 80
    when "Backstage passes to a TAFKAL80ETC concert"
      update_backstage_passes(item)
    else
      update_standard_item(item)
    end
    item.sell_in -= 1 unless item.name == "Sulfuras, Hand of Ragnaros"
    handle_expired(item)
  end

  def validate_item(item)
    raise StandardError, "Invalid sell_in value" unless item.sell_in.is_a?(Integer)
    raise StandardError, "Invalid quality value" unless item.quality.is_a?(Integer)
  end

  def update_aged_brie(item)
    increase_quality(item)
  end

  def update_backstage_passes(item)
    increase_quality(item)
    increase_quality(item) if item.sell_in <= 10
    increase_quality(item) if item.sell_in <= 5
  end

  def update_standard_item(item)
    decrease_quality(item)
  end

  def handle_expired(item)
    return unless item.sell_in < 0

    case item.name
    when "Aged Brie"
      increase_quality(item)
    when "Backstage passes to a TAFKAL80ETC concert"
      item.quality = 0
    else
      decrease_quality(item) unless item.name == "Sulfuras, Hand of Ragnaros"
    end
  end

  def increase_quality(item)
    item.quality += 1 if item.quality < 50
  end

  def decrease_quality(item)
    item.quality -= 1 if item.quality > 0
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
