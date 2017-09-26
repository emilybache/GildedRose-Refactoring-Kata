require 'ostruct'

class Item < OpenStruct
  def to_s
    [name, sell_in, quality].join ', '
  end
end
