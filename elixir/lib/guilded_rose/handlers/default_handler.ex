defmodule DefaultHandler do
  alias Utils.Quality
  @behaviour ItemHandler

  def is_handled?(item_name), do: true
    
  def handle(item = %{ sell_in: sell_in }) when sell_in - 1 < 0, do:
    %{item | sell_in: sell_in - 1, quality: Quality.calculate(item.quality - 2) }

  def handle(item), do:
    %{item | sell_in: item.sell_in - 1, quality: Quality.calculate(item.quality - 1) }

end