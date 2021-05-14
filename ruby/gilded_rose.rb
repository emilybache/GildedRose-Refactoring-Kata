# frozen_string_literal: true

class GildedRose

  AGED_BRIE = "Aged Brie"
  BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  SULFURAS = "Sulfuras, Hand of Ragnaros"
  QUALITY_THRESHOLD = 50

  def initialize(items)
    @items = items
  end

  # - AGED_BRIEは時間とともに品質が上がり、販売期限が過ぎるとさらに品質が上がる
  # - SULFURASについては何もしない
  # - 通常のitemは徐々に品質が下がり、販売期限が下がるとさらに品質が下がる
  # - アイテムの品質は50まで
  # - item quority は 0以下にはならないk
  # - BACKSTAGE_PASSは、sell in  11 以上のときは + 1
  # - BACKSTAGE_PASSは、sell in  6-10のときは + 2
  # - BACKSTAGE_PASSは、sell in  6 以下のときは + 3

  def update_quality()
    @items.each do |item|
      next if item.name == SULFURAS

      first_step(item)
      item.sell_in = item.sell_in - 1
      next if item.sell_in >= 0
      case item.name
      when AGED_BRIE
        increment_item_quality(item)
      when BACKSTAGE_PASS
          item.quality = 0
      else
        if item.quality > 0
          item.quality = item.quality - 1
        end
      end
    end
  end

  def first_step(item)
    case item.name
    when AGED_BRIE
      increment_item_quality(item)
    when BACKSTAGE_PASS
      increment_amount = case item.sell_in
      when (11..) then 1
      when (6...11) then 2
      when (...6) then 3
      end
      increment_item_quality(item, increment_amount)
    else
      if item.quality > 0
        item.quality = item.quality - 1
      end
    end
  end

  def increment_item_quality(item, num = 1)
    item.quality = [item.quality + num, QUALITY_THRESHOLD].min
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
