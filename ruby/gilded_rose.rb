class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    non_degrading_items = ["Aged Brie", "Backstage passes to a TAFKAL80ETC concert", "Sulfuras, Hand of Ragnaros"]# Do not degrade in quality
    
    item_degradation_map = {
      # Item Type (substring to look for) | Quality Increment Per Day
      "Aged Brie": 1,
      "Backstage passes": {
        default: 1,
        ten_days_or_less: 2,
        five_days_or_less: 3
      },
      "Conjured": -2,
      "Sulfuras": 0,
      "Default": -1
    }

    @items.each do |item|

      # Item Quality can not be negative or over 50

      quality_update_allowed = item.quality == 0 || item.quality > 50 ? false : true
      item_expired = item.sell_in < 0 ? true : false

      if quality_update_allowed
        # Find the item type by substring
        puts "hi"
      end

      # case item.name
      # when "Aged Brie"

      # when "Backstage passes to a TAFKAL80ETC concert"

      # when "Sulfuras, Hand of Ragnaros"

      # else

      # end
      # if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert"
      #   if item.quality > 0
      #     if item.name != "Sulfuras, Hand of Ragnaros"
      #       item.quality = item.quality - 1
      #     end
      #   end
      # else
      #   if item.quality < 50
      #     item.quality = item.quality + 1
      #     if item.name == "Backstage passes to a TAFKAL80ETC concert"
      #       if item.sell_in < 11
      #         if item.quality < 50
      #           item.quality = item.quality + 1
      #         end
      #       end
      #       if item.sell_in < 6
      #         if item.quality < 50
      #           item.quality = item.quality + 1
      #         end
      #       end
      #     end
      #   end
      # end
      # if item.name != "Sulfuras, Hand of Ragnaros"
      #   item.sell_in = item.sell_in - 1
      # end
      # if item.sell_in < 0
      #   if item.name != "Aged Brie"
      #     if item.name != "Backstage passes to a TAFKAL80ETC concert"
      #       if item.quality > 0
      #         if item.name != "Sulfuras, Hand of Ragnaros"
      #           item.quality = item.quality - 1
      #         end
      #       end
      #     else
      #       item.quality = item.quality - item.quality
      #     end
      #   else
      #     if item.quality < 50
      #       item.quality = item.quality + 1
      #     end
      #   end
      # end
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

  def to_s()
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
