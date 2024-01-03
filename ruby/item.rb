class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name, sell_in, quality)
    @name = name  # Name of the item
    @sell_in = sell_in  # Number of days to sell the item
    @quality = quality  # Represents how valuable the item is
  end

  def to_s()
    "#{@name}, #{@sell_in}, #{@quality}"  # Representing the item's details as a string
  end
end
