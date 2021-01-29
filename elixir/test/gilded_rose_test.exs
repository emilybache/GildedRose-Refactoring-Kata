defmodule GildedRoseTest do
  use ExUnit.Case

  import GildedRose

  @brie "Aged Brie"
  @sulfuras "Sulfuras, Hand of Ragnaros"
  @concert_ticket "Backstage passes to a TAFKAL80ETC concert"

  def create_items(sell_in, quality) do
    [
      %Item{name: @sulfuras, sell_in: sell_in, quality: quality},
      %Item{name: @concert_ticket, sell_in: sell_in, quality: quality},
      %Item{name: @brie, sell_in: sell_in, quality: quality},
    ]
  end

  def elapse_days(items, 0), do: items
  
  def elapse_days(items, days) do
    elapse_days(update_quality(items), days - 1)
  end

  # Price degradation as tests pass

  test "all items quality _ 11 or more days left" do
    days_left = 11
    initial_quality = 0
    result = create_items(days_left, initial_quality) |> elapse_days(1)

    assert result === [
      %Item{name: @sulfuras, sell_in: 11, quality: 0},
      %Item{name: @concert_ticket, sell_in: 10, quality: 1},
      %Item{name: @brie, sell_in: 10, quality: 1}
    ]
  end

  test "ticket quality _ 10 days left" do
    days_left = 10
    initial_quality = 0
    [_, ticket, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert ticket === %Item{name: @concert_ticket, sell_in: 9, quality: 2}
  end

  test "ticket quality _ 5 days leff" do
    days_left = 5
    initial_quality = 0
    [_, ticket, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert ticket === %Item{name: @concert_ticket, sell_in: 4, quality: 3}
  end

  test "ticket quality _ 0 days left" do
    days_left = 0
    initial_quality = 0
    [_, ticket, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert ticket === %Item{name: @concert_ticket, sell_in: -1, quality: 0}
  end

  test "brie quality _ greater than 0 days left " do
    days_left = 1
    initial_quality = 0
    [_, _, brie]  = create_items(days_left, initial_quality) |> elapse_days(1)

    assert brie === %Item{name: @brie, sell_in: 0, quality: 1}
  end


  test "brie quality _ 0 days left" do
    days_left = 0
    initial_quality = 0
    [_, _, brie]  = create_items(days_left, initial_quality) |> elapse_days(1)

    assert brie === %Item{name: @brie, sell_in: -1, quality: 2}
  end

  test "sulfuras quality _ any days left" do
    days_left = 20
    initial_quality = 40
    [sulfuras, _, _]  = create_items(days_left, initial_quality) |> elapse_days(40)

    assert sulfuras === %Item{name: @sulfuras, sell_in: 20, quality: 40}
  end

end
