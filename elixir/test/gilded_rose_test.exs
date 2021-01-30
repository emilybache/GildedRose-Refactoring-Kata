defmodule GildedRoseTest do
  use ExUnit.Case

  import GildedRose

  @brie     "Aged Brie"
  @conjured "Conjured"
  @normal   "Normal Item"
  @sulfuras "Sulfuras, Hand of Ragnaros"
  @passes   "Backstage passes to a TAFKAL80ETC concert"

  def create_items(sell_in, quality) do
    [
      %Item{name: @sulfuras, sell_in: sell_in, quality: 80},
      %Item{name: @passes, sell_in: sell_in, quality: quality},
      %Item{name: @brie, sell_in: sell_in, quality: quality},
      %Item{name: @normal, sell_in: sell_in, quality: quality},
      %Item{name: @conjured, sell_in: sell_in, quality: quality},
    ]
  end

  def elapse_days(items, 0), do: items
  
  def elapse_days(items, days) do
    elapse_days(update_quality(items), days - 1)
  end

  test "all items quality _ 11 days or more left" do
    days_left = 12
    initial_quality = 2
    result = create_items(days_left, initial_quality) |> elapse_days(1)

    assert result === [
      %Item{name: @sulfuras, sell_in: 12, quality: 80},
      %Item{name: @passes, sell_in: 11, quality: 3},
      %Item{name: @brie, sell_in: 11, quality: 3},
      %Item{name: @normal, sell_in: 11, quality: 1},
      %Item{name: @conjured, sell_in: 11, quality: 0},
    ]
  end

  # Normal items

  test "normal item quality _ any day greater than 0" do
    days_left = 10
    initial_quality = 10
    [_, _, _, normal, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert normal === %Item{name: @normal, sell_in: 9, quality: 9}
  end

  test "normal item quality _ last day" do
    days_left = 1
    initial_quality = 5
    [_, _, _, normal, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert normal === %Item{name: @normal, sell_in: 0, quality: 4}
  end
  
  test "normal item quality _ no days left" do
    days_left = 0
    initial_quality = 4
    [_, _, _, normal, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert normal === %Item{name: @normal, sell_in: -1, quality: 2}
  end

  test "normal item quality _ no days left _ no more quality" do
    days_left = 0
    initial_quality = 4
    [_, _, _, normal, _] = create_items(days_left, initial_quality) |> elapse_days(4)

    assert normal === %Item{name: @normal, sell_in: -4, quality: 0}
  end
  
  # Conjured items

  test "conjured item quality _ any day greater than 0" do
    days_left = 10
    initial_quality = 10
    [_, _, _, _, conjured] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert conjured === %Item{name: @conjured, sell_in: 9, quality: 8}
  end

  test "conjured item quality _ last day" do
    days_left = 1
    initial_quality = 5
    [_, _, _, _, conjured] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert conjured === %Item{name: @conjured, sell_in: 0, quality: 3}
  end
  
  test "conjured item quality _ no days left" do
    days_left = 0
    initial_quality = 8
    [_, _, _, _, conjured] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert conjured === %Item{name: @conjured, sell_in: -1, quality: 4}
  end

  test "conjured item quality _ no days left _ no more quality" do
    days_left = 0
    initial_quality = 4
    [_, _, _, _, conjured] = create_items(days_left, initial_quality) |> elapse_days(4)

    assert conjured === %Item{name: @conjured, sell_in: -4, quality: 0}
  end


  # Passes


  test "passes quality _ 10 days or less" do
    days_left = 10
    initial_quality = 0
    [_, passes, _, _, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert passes === %Item{name: @passes, sell_in: 9, quality: 2}
  end

  test "passes quality _ 5 days or less" do
    days_left = 5
    initial_quality = 0
    [_, passes, _, _, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert passes === %Item{name: @passes, sell_in: 4, quality: 3}
  end

  test "passes quality _ after concert" do
    days_left = 0
    initial_quality = 0
    [_, passes, _, _, _] = create_items(days_left, initial_quality) |> elapse_days(1)

    assert passes === %Item{name: @passes, sell_in: -1, quality: 0}
  end
  
  # Brie

  test "brie quality _ greater than 0 days left " do
    days_left = 1
    initial_quality = 0
    [_, _, brie, _, _]  = create_items(days_left, initial_quality) |> elapse_days(1)

    assert brie === %Item{name: @brie, sell_in: 0, quality: 1}
  end

  test "brie quality _ past sell in day" do
    days_left = 0
    initial_quality = 0
    [_, _, brie, _, _]  = create_items(days_left, initial_quality) |> elapse_days(1)

    assert brie === %Item{name: @brie, sell_in: -1, quality: 2}
  end

  # Sulfuras

  test "sulfuras quality _ any days left" do
    days_left = 20
    initial_quality = 40
    [sulfuras, _, _, _, _]  = create_items(days_left, initial_quality) |> elapse_days(40)

    assert sulfuras === %Item{name: @sulfuras, sell_in: 20, quality: 80}
  end

  test "sulfuras quality _ lengendary quality" do
    days_left = 20
    initial_quality = 100
    [sulfuras, _, _, _, _]  = create_items(days_left, initial_quality) |> elapse_days(1)

    assert sulfuras === %Item{name: @sulfuras, sell_in: 20, quality: 80}
  end

  # Max quality

  test "all items quality _ reaching max quality" do
    days_left = 5
    initial_quality = 49
    result = create_items(days_left, initial_quality) |> elapse_days(5)

    assert result === [
      %Item{name: @sulfuras, sell_in: 5, quality: 80},
      %Item{name: @passes, sell_in: 0, quality: 50},
      %Item{name: @brie, sell_in: 0, quality: 50},
      %Item{name: @normal, sell_in: 0, quality: 44},
      %Item{name: @conjured, sell_in: 0, quality: 39},
    ]
  end
end
