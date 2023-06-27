class NormalItem
  attr_reader :item

  def initialize(item)
    @item = item
  end

  def spend_day_in_shop
    decrease_sell_in_day

    decrease_quality
    decrease_quality if item.sell_in.negative?

    keep_quality_lower_limit_in_bounds
    keep_quality_upper_limit_in_bounds

    self
  end

  private

    def decrease_sell_in_day
      item.sell_in -= 1
    end

    def decrease_quality
      item.quality -= 1
    end

    def increase_quality
      item.quality += 1
    end

    def keep_quality_lower_limit_in_bounds
      item.quality = GildedRose::QUALITY_LOWER_LIMIT if item.quality < GildedRose::QUALITY_LOWER_LIMIT
    end

    def keep_quality_upper_limit_in_bounds
      item.quality = GildedRose::QUALITY_UPPER_LIMIT if item.quality > GildedRose::QUALITY_UPPER_LIMIT
    end
end
