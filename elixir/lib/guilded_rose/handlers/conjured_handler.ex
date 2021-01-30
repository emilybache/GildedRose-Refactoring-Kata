defmodule ConjuredHandler do
  alias Utils.Quality
  @behaviour ItemHandler
  @item_name "Conjured"

  def is_handled?(item_name), do: item_name == @item_name

  def handle(item = %{name: @item_name, sell_in: sell_in }) when sell_in - 1 < 0, do:
    %{item | sell_in: sell_in - 1, quality: Quality.calculate(item.quality - 4) }

  def handle(item = %{name: @item_name}), do: 
    %{item | sell_in: item.sell_in - 1, quality: Quality.calculate(item.quality - 2) }

end