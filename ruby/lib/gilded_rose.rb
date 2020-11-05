class GildedRose

  # def initialize(items)
  #   @items = items
  # end

  def self.update_quality(items)
    items.map do |item|
      if !special_item?(item)
          
            update_normal_quality(item) if !sulfuras?(item)

      else
        # start of block for brie and backstage quality logic
        if item.quality < 50
          item.quality = item.quality + 1
          if item.name.downcase.match /backstage/
            if item.sell_in < 11

                item.quality = item.quality + 1

            end
            if item.sell_in < 6

                item.quality = item.quality + 1

            end
          end
        end
        # end of block for brie and backstage quality logic
      end

      # start of block that reduces sell in 
      if !sulfuras?(item)
        item.sell_in = item.sell_in - 1
      end
      # end of block that reduces sell in 


      if item.sell_in < 0

        if item.name != "Aged Brie"

          if !item.name.downcase.match /backstage/
            update_normal_quality(item) unless sulfuras?(item)

          else
            item.quality = 0
          end
        else
          if item.quality < 50
            item.quality = item.quality + 1
          end
        end



      end
    end
  end

  def self.update_normal_quality(item)
    item.quality -= 1 unless item.quality.zero?
  end

  def self.sulfuras?(item)
    !item.name.downcase.match( /sulfuras/).nil?
  end

  def self.special_item?(item)
   ( !item.name.downcase.match( /aged brie/).nil? ||  !item.name.downcase.match(/backstage/).nil? || sulfuras?(item))
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
