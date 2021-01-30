defmodule PassesHandler do
  alias Utils.Quality
  @behaviour ItemHandler
  @item_name "Backstage passes to a TAFKAL80ETC concert"

  def is_handled?(item_name), do: item_name == @item_name

  def handle(item = %{ name: @item_name , sell_in: days }) when days - 1 < 0, do:
    %{item | sell_in: days - 1, quality: 0 }

  def handle(item = %{ name: @item_name , sell_in: days, quality: quality }) when days - 1 < 5, do:
    %{item | sell_in: days - 1, quality: Quality.calculate(quality + 3) }

  def handle(item = %{ name: @item_name , sell_in: days, quality: quality }) when days - 1 < 10, do:
    %{item | sell_in: days - 1, quality: Quality.calculate(quality + 2) }

  def handle(item = %{ name: @item_name , sell_in: days, quality: quality }), do:
    %{item | sell_in: days - 1, quality: Quality.calculate(quality + 1) }

end