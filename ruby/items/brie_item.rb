require_relative 'normal_item'

class BrieItem < NormalItem
  def spend_day_in_shop
    decrease_sell_in_day

    increase_quality
    increase_quality if item.sell_in.negative?

    keep_quality_upper_limit_in_bounds

    self
  end
end
