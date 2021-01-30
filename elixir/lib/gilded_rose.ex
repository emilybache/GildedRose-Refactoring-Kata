defmodule GildedRose do

  def update_quality(items) do
    Enum.map(items, &update_item/1)
  end

  def update_item(item = %{ name: name }) do
    cond do
      PassesHandler.is_handled?(name) ->
        PassesHandler.handle(item)

      BrieHandler.is_handled?(name) ->
        BrieHandler.handle(item)

      SulfurasHandler.is_handled?(name) -> 
        SulfurasHandler.handle(item)

      ConjuredHandler.is_handled?(name) ->
        ConjuredHandler.handle(item)

      DefaultHandler.is_handled?(name) ->
        DefaultHandler.handle(item)
    end
  end

  # # Passes

  # @ticket "Backstage passes to a TAFKAL80ETC concert"
  # def update_item_new(item = %{ name: @ticket , sell_in: days }) when days - 1 < 0, do:
  #   %{item | sell_in: days - 1, quality: 0 }

  # def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 < 5, do:
  #   %{item | sell_in: days - 1, quality: updated_quality(quality + 3) }

  # def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 < 10, do:
  #   %{item | sell_in: days - 1, quality: updated_quality(quality + 2) }

  # def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }), do:
  #   %{item | sell_in: days - 1, quality: updated_quality(quality + 1) }

  
  # # Brie

  # @brie "Aged Brie"
  # def update_item_new(item = %{ name: @brie, sell_in: days, quality: quality }) when  days - 1 < 0,  do:
  #   %{item | sell_in: days - 1, quality: updated_quality(quality + 2) }

  # def update_item_new(item = %{ name: @brie, sell_in: days, quality: quality }), do:
  #   %{item | sell_in: days - 1, quality: updated_quality(quality + 1) }
    

  # # Legendary

  # @sulfuras "Sulfuras, Hand of Ragnaros"
  # def update_item_new(item = %{ name: @sulfuras }), do:
  #   %{item | quality: 80 }


  # # Conjured

  # @conjured "Conjured"
  # def update_item_new(item = %{name: @conjured, sell_in: sell_in }) when sell_in - 1 < 0, do:
  #   %{item | sell_in: sell_in - 1, quality: updated_quality(item.quality - 4) }

  # def update_item_new(item = %{name: @conjured}), do: 
  #   %{item | sell_in: item.sell_in - 1, quality: updated_quality(item.quality - 2) }

  # # Normal
  
  # def update_item_new(item = %{sell_in: sell_in }) when sell_in - 1 < 0, do:
  #   %{item | sell_in: sell_in - 1, quality: updated_quality(item.quality - 2) }

  # def update_item_new(item), do:
  #   %{item | sell_in: item.sell_in - 1, quality: updated_quality(item.quality - 1) }

  # # Utils

  # defp updated_quality(new_amount)
  # defp updated_quality(new_amount) when new_amount < 0, do: 0
  # defp updated_quality(new_amount) when new_amount > @max_quality, do: @max_quality
  # defp updated_quality(new_amount), do: new_amount

end
