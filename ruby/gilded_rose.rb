class GildedRose
  
  def self.all_items
    ObjectSpace.each_object(Item).to_a  
  end
  
  def self.update_quality
    all_items.each do |item|
      if !item.name.include?("Aged Brie") and !item.name.include?("Backstage")
        if item.quality > 0
          if !item.name.include?("Sulfuras")
            item.quality -= 1
            if item.name.include?("Conjured")
              item.quality -= 1
            end 
          end
        end
      else
        if item.quality < 50
          item.quality += 1
          if item.name.include?("Backstage")
            if item.sell_in < 11
              if item.quality < 50
                item.quality += 1
              end
            end
            if item.sell_in < 6
              if item.quality < 50
                item.quality += 1
              end
            end
          end
        end
      end
      if !item.name.include?("Sulfuras")
        item.sell_in = item.sell_in - 1
      end
      if item.sell_in < 0
        if !item.name.include?("Aged Brie") 
          if !item.name.include?("Backstage")
            if item.quality > 0
              if !item.name.include?("Sulfuras")
                item.quality -= 1
                if item.name.include?("Conjured")
                  item.quality -= 1
                end
              end
            end
          else
            item.quality = 0
          end
        else
          if item.quality < 50
            item.quality += 1
          end
        end
      end
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