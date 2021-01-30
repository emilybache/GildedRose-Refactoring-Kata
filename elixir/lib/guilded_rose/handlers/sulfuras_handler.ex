defmodule SulfurasHandler do
  @behaviour ItemHandler
  @item_name "Sulfuras, Hand of Ragnaros"

  def is_handled?(item_name), do: item_name == @item_name

  def handle(item = %{ name: @item_name }), do:
    %{item | quality: 80 }

end