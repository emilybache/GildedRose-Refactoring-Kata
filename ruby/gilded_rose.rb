require 'byebug'
class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each do |item|

      # Decreasing the quality of item
      if !item.is_aged_brie? && !item.is_backstage_passes? && !item.is_sulfuras? && item.quality > 0
        if item.sell_in < 0
          item.quality = item.quality - 4 if item.is_conjured_mana_cake? # Decreasing twice quality if item is conjured_mana_cake
          item.quality = item.quality - 2 if !item.is_conjured_mana_cake? # Decreasing 2 quality if item is not conjured_mana_cake
        else
          item.quality = item.quality - 2 if item.is_conjured_mana_cake? # Decreasing twice quality if item is conjured_mana_cake
          item.quality = item.quality - 1 if !item.is_conjured_mana_cake? # Decreasing 2 quality if item is not conjured_mana_cake
        end
      elsif item.quality < 50 && (item.is_backstage_passes? || item.is_aged_brie?)
        # Increasing quality for backstage passes and aged brie item according based on sell in days
        if item.sell_in < 11
          item.quality = item.quality + 2
        elsif item.sell_in < 6
          item.quality = item.quality + 3
        else 
          item.quality = item.quality + 1
        end
        item.quality = 50 if item.quality > 50 # Insuring item quality should not get over 50
        item.quality = 0 if item.quality < 0 # Insuring item quality should not be negative
      end

      if !item.is_sulfuras?
        item.sell_in = item.sell_in - 1 # Decreasing sell in daily if item is not sulfuras
      end

      if item.sell_in < 0
        # Setting quality to 0 if concert is over
        item.quality = item.quality - item.quality if item.is_aged_brie? || item.is_backstage_passes?
      end
      item.quality = 50 if item.quality > 50 && item.is_sulfuras?
    end
  end
end
