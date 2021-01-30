defmodule GildedRose do

  def update_quality(items) do
    Enum.map(items, &update_item_new/1)
  end

  @ticket "Backstage passes to a TAFKAL80ETC concert"
  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 < 0, do:
    %{item | sell_in: days - 1, quality: 0 }

  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 <= 5, do:
    %{item | sell_in: days - 1, quality: updated_quality(50, quality + 3) }

  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) when days - 1 <= 10, do:
    %{item | sell_in: days - 1, quality: updated_quality(50, quality + 2) }

  def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }), do:
    %{item | sell_in: days - 1, quality: updated_quality(50, quality + 1) }


  # def update_item_new(item = %{ name: @ticket , sell_in: days, quality: quality }) do
  #   left = days - 1 

  #   cond do
  #     left > 5 && left < 10 ->  %{item | sell_in: left, quality: updated_quality(50, quality + 2) }
  #     left > 0 && left < 5 ->   %{item | sell_in: left, quality: updated_quality(50, quality + 3) }
  #     left < 0 ->               %{item | sell_in: left, quality: 0 }
  #     true ->                   %{item | sell_in: left, quality: updated_quality(50, quality + 1) }
  #   end
  # end

  @brie "Aged Brie"
  def update_item_new(item = %{ name: @brie, sell_in: days, quality: quality }) when  days - 1 < 0,  do:
    %{item | sell_in: days - 1, quality: updated_quality(50, quality + 2) }

  def update_item_new(item = %{ name: @brie, sell_in: days, quality: quality }), do:
    %{item | sell_in: days - 1, quality: updated_quality(50, quality + 1) }
    

  @sulfuras "Sulfuras, Hand of Ragnaros"
  def update_item_new(item = %{ name: @sulfuras, quality: quality }) when quality > 80, do: %{item | quality: 80 }
  def update_item_new(item = %{ name: @sulfuras }), do: item


  defp updated_quality(max, new_amount) when new_amount > max, do: max
  defp updated_quality(_, new_amount), do: new_amount






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
