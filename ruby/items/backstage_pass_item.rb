require_relative 'normal_item'

class BackstagePassItem < NormalItem
  def spend_day_in_shop
    decrease_sell_in_day
    return item.quality = 0 if item.sell_in.negative?

    increase_quality
    increase_quality if item.sell_in < 10
    increase_quality if item.sell_in < 5

    keep_quality_upper_limit_in_bounds

    self
  end
end
