defmodule GildedRose do
  # Example
  # update_quality([%Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 9, quality: 1}])
  # => [%Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 9, quality: 3}]

  def update_quality(items) do
    Enum.map(items, &update_item/1)
  end

  def update_item(item) do
    cond do
      item.quality == 0 ->
        item
      item.sell_in < 0 && item.name == "Backstage passes to a TAFKAL80ETC concert" ->
        %{item | quality: 0}
      item.name == "Aged Brie" || item.name == "Backstage passes to a TAFKAL80ETC concert" ->
        if item.name == "Backstage passes to a TAFKAL80ETC concert" && item.sell_in > 5 && item.sell_in <= 10 do
          %{item | quality: item.quality + 2}
        else
          if item.name == "Backstage passes to a TAFKAL80ETC concert" && item.sell_in >= 0 && item.sell_in <= 5 do
            %{item | quality: item.quality + 3}
          else
            if item.quality < 50 do
              %{item | quality: item.quality + 1}
            else
              item
            end
          end
        end
      item.sell_in < 0 ->
        if item.name == "Backstage passes to a TAFKAL80ETC concert" do
          %{item | quality: 0}
        else
          if item.name == "+5 Dexterity Vest" || item.name == "Elixir of the Mongoose" do
            %{item | quality: item.quality - 2}
          else
            item
          end
        end
      item.name == "+5 Dexterity Vest" || item.name == "Elixir of the Mongoose" ->
        %{item | quality: item.quality - 1}
      item.name != "Sulfuras, Hand of Ragnaros" ->
        %{item | quality: item.quality - 1}
      true ->
        item
    end
  end
end
