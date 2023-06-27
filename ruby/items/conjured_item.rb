require_relative 'normal_item'

class ConjuredItem< NormalItem
  def spend_day_in_shop
    decrease_sell_in_day

    decrease_quality
    decrease_quality

    if item.sell_in.negative?
      decrease_quality
      decrease_quality
    end

    keep_quality_lower_limit_in_bounds

    self
  end
end
