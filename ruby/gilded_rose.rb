class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      if GildedRose.private_instance_methods.to_s.include?("#{correct_name(item)}_update!")
        send("#{correct_name(item)}_update!", item)
      else
        classic_item_update(item)
      end
    end
  end

  private

  def correct_name(item)
    item.name.split(/\W+/).join('_').downcase
  end

  def classic_item_update(item)
    item.sell_in -= 1
    item.sell_in < 0 ? item.quality -= 1 : item.quality -= 2
  end

  def aged_brie_update!(item)
    item.sell_in -= 1
    item.sell_in < 0 ? item.quality += item.sell_in.abs : item.quality += 1
    item.quality = 50 if item.quality > 50
  end

  def sulfuras_hand_of_ragnaros_update!(item)
    # nothing method
  end

  def backstage_passes_to_a_tafkal80etc_concert_update!(item)
    item.sell_in -= 1
    item.quality += 1 if item.quality < 50
    item.quality += 1 if item.sell_in < 11 && item.quality < 50
    item.quality += 1 if item.sell_in < 6 && item.quality < 50
  end

  def conjured_mana_cake_update!(item)
    item.sell_in -= 1
    item.sell_in >= 0 ? item.quality -= 2 : item.quality -= 4
  end
end

class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name, sell_in, quality)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end

  def to_s
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
