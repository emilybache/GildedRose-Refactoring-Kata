defmodule GildedRose do

  @max_quality 50

  def update_quality(items) do
    Enum.map(items, &update_item_new/1)
  end

  # Passes

  @ticket "Backstage passes to a TAFKAL80ETC concert"
  def update_item_new(item = %{ name: @ticket , sell_in: days }) when days - 1 < 0, do:
    %{item | sell_in: days - 1, quality: 0 }

  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 < 5, do:
    %{item | sell_in: days - 1, quality: updated_quality(quality + 3) }

  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 < 10, do:
    %{item | sell_in: days - 1, quality: updated_quality(quality + 2) }

  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }), do:
    %{item | sell_in: days - 1, quality: updated_quality(quality + 1) }

  
  # Brie

  @brie "Aged Brie"
  def update_item_new(item = %{ name: @brie, sell_in: days, quality: quality }) when  days - 1 < 0,  do:
    %{item | sell_in: days - 1, quality: updated_quality(quality + 2) }

  def update_item_new(item = %{ name: @brie, sell_in: days, quality: quality }), do:
    %{item | sell_in: days - 1, quality: updated_quality(quality + 1) }
    

  # Legendary

  @sulfuras "Sulfuras, Hand of Ragnaros"
  def update_item_new(item = %{ name: @sulfuras }), do:
    %{item | quality: 80 }


  # Conjured

  @conjured "Conjured"
  def update_item_new(item = %{name: @conjured, sell_in: sell_in }) when sell_in - 1 < 0, do:
    %{item | sell_in: sell_in - 1, quality: updated_quality(item.quality - 4) }

  def update_item_new(item = %{name: @conjured}), do: 
    %{item | sell_in: item.sell_in - 1, quality: updated_quality(item.quality - 2) }

  # Normal
  
  def update_item_new(item = %{sell_in: sell_in }) when sell_in - 1 < 0, do:
    %{item | sell_in: sell_in - 1, quality: updated_quality(item.quality - 2) }

  def update_item_new(item), do:
    %{item | sell_in: item.sell_in - 1, quality: updated_quality(item.quality - 1) }

  # Utils

  defp updated_quality(new_amount)
  defp updated_quality(new_amount) when new_amount < 0, do: 0
  defp updated_quality(new_amount) when new_amount > @max_quality, do: @max_quality
  defp updated_quality(new_amount), do: new_amount


  def update_item(item) do
    item = cond do
      item.name != "Aged Brie" && item.name != "Backstage passes to a TAFKAL80ETC concert" ->
        if item.quality > 0 do
          if item.name != "Sulfuras, Hand of Ragnaros" do
            %{item | quality: item.quality - 1}
          else
            item
          end
        else
          item
        end
      true ->
        cond do
          item.quality < 50 ->
            item = %{item | quality: item.quality + 1}
            cond do
              item.name == "Backstage passes to a TAFKAL80ETC concert" ->
                item = cond do
                  item.sell_in < 11 ->
                    cond do
                      item.quality < 50 ->
                        %{item | quality: item.quality + 1}
                      true -> item
                    end
                  true -> item
                end
                cond do
                  item.sell_in < 6 ->
                    cond do
                      item.quality < 50 ->
                        %{item | quality: item.quality + 1}
                      true -> item
                    end
                  true -> item
                end
              true -> item
            end
          true -> item
        end
    end
    item = cond do
      item.name != "Sulfuras, Hand of Ragnaros" ->
        %{item | sell_in: item.sell_in - 1}
      true -> item
    end
    cond do
      item.sell_in < 0 ->
        cond do
          item.name != "Aged Brie" ->
            cond do
              item.name != "Backstage passes to a TAFKAL80ETC concert" ->
                cond do
                  item.quality > 0 ->
                    cond do
                      item.name != "Sulfuras, Hand of Ragnaros" ->
                        %{item | quality: item.quality - 1}
                      true -> item
                    end
                  true -> item
                end
              true -> %{item | quality: item.quality - item.quality}
            end
          true ->
            cond do
              item.quality < 50 ->
                %{item | quality: item.quality + 1}
              true -> item
            end
        end
      true -> item
    end
  end
end
