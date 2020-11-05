# frozen_string_literal: true

# class for gildedrose item degradation
class GildedRose
  def self.update_quality(items)
    items.map do |item|
      case
      when !special_item?(item)
        update_normal_quality(item)
        item.sell_in -= 1
      when backstage?(item)
        update_backstage_quality(item) if item.quality < 50
        item.sell_in -= 1
      when brie?(item)
        update_brie_quality(item)
        item.sell_in -= 1
      when conjured?(item)
        2.times { update_normal_quality(item) }
      end
    end
  end

  def self.update_backstage_quality(item)
    case item.sell_in
    when (-Float::INFINITY..0)
      item.quality = 0
    when 0..5
      3.times { item.quality += 1 if item.quality < 50 }
    when 6..10
      2.times { item.quality += 1 if item.quality < 50 }
    when 10..Float::INFINITY
      item.quality += 1
    end
  end

  def self.update_normal_quality(item)
    if item.sell_in.negative?
      2.times {  item.quality -= 1 unless item.quality.zero? }
    else
      item.quality -= 1 unless item.quality.zero?
    end
  end

  def self.update_brie_quality(item)
    if item.sell_in < 1 && item.quality < 48
      item.quality += 2
    else
      item.quality += 1 if item.quality < 50
    end
  end

  def self.sulfuras?(item)
    !item.name.downcase.match(/sulfuras/).nil?
  end

  def self.brie?(item)
    !item.name.downcase.match(/aged brie/).nil?
  end

  def self.backstage?(item)
    !item.name.downcase.match(/backstage/).nil?
  end

  def self.conjured?(item)
    !item.name.downcase.match(/conjured/).nil?
  end

  def self.special_item?(item)
    (brie?(item) || backstage?(item) || conjured?(item) || sulfuras?(item))
  end
end
# class for items owned by gilded rose inn
class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name, sell_in, quality)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end

  # def to_s
  #   "#{@name}, #{@sell_in}, #{@quality}"
  # end
end
