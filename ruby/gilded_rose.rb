# frozen_string_literal: true

require File.join(File.dirname(__FILE__), 'item_processor')

class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      ItemProcessor.new(item).update_item_quality
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

  def to_s
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
