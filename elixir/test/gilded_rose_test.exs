defmodule GildedRoseTest do
  use ExUnit.Case

  import GildedRose

  @brie "Aged Brie"
  @sulfuras "Sulfuras, Hand of Ragnaros"
  @concert_ticket "Backstage passes to a TAFKAL80ETC concert"

  def create_item() do
    [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 13, quality: 1},
      %Item{name: @brie, sell_in: 13, quality: 1},
    ]
  end

  def elapse_days(items, 0), do: items
  
  def elapse_days(items, days) do
    elapse_days(update_quality(items), days - 1)
  end

  test "1 day pass" do
    result = create_item() |> elapse_days(1)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 12, quality: 2},
      %Item{name: @brie, sell_in: 12, quality: 2}
    ]
  end

  test "2 day pass" do
    result = create_item() |> elapse_days(2)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 11, quality: 3},
      %Item{name: @brie, sell_in: 11, quality: 3}
    ]
  end
  
  test "3 day pass" do
    result = create_item() |> elapse_days(3)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 10, quality: 4},
      %Item{name: @brie, sell_in: 10, quality: 4}
    ]
  end

  test "4 day pass" do
    result = create_item() |> elapse_days(4)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 9, quality: 6}, # Here the tickets start increasing by 2
      %Item{name: @brie, sell_in: 9, quality: 5}            
    ]
  end

  test "5 day pass" do
    result = create_item() |> elapse_days(5)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 8, quality: 8},
      %Item{name: @brie, sell_in: 8, quality: 6}
    ]         
  end
  

  test "6 day pass" do
    result = create_item() |> elapse_days(6)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 7, quality: 10},
      %Item{name: @brie, sell_in: 7, quality: 7}
    ]      
  end

  test "7 day pass" do
    result = create_item() |> elapse_days(7)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 6, quality: 12},
      %Item{name: @brie, sell_in: 6, quality: 8}
    ]
  end

  test "8 day pass" do
    result = create_item() |> elapse_days(8)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 5, quality: 14},
      %Item{name: @brie, sell_in: 5, quality: 9}
    ]
  end

  test "9 day pass" do
    result = create_item() |> elapse_days(9)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 4, quality: 17},
      %Item{name: @brie, sell_in: 4, quality: 10}
    ]
  end

  test "10 day pass" do
    result = create_item() |> elapse_days(10)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 3, quality: 20},
      %Item{name: @brie, sell_in: 3, quality: 11}
    ]
  end

  test "11 day pass" do
    result = create_item() |> elapse_days(11)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 2, quality: 23},
      %Item{name: @brie, sell_in: 2, quality: 12}
    ]
  end

  test "12 day pass" do
    result = create_item() |> elapse_days(12)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 1, quality: 26},
      %Item{name: @brie, sell_in: 1, quality: 13}
    ]
  end

  test "13 day pass" do
    result = create_item() |> elapse_days(13)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: 0, quality: 29},
      %Item{name: @brie, sell_in: 0, quality: 14}
    ]
  end

  test "14 day pass" do
    result = create_item() |> elapse_days(14)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -1, quality: 0},
      %Item{name: @brie, sell_in: -1, quality: 16}            # After days are over brie quality doubles
    ]
  end

  test "15 day pass" do
    result = create_item() |> elapse_days(15)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -2, quality: 0},
      %Item{name: @brie, sell_in: -2, quality: 18},
    ]
  end

  test "16 day pass" do
    result = create_item() |> elapse_days(16)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -3, quality: 0},
      %Item{name: @brie, sell_in: -3, quality: 20},
    ]
  end

  test "17 day pass" do
    result = create_item() |> elapse_days(17)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -4, quality: 0},
      %Item{name: @brie, sell_in: -4, quality: 22},
    ]
  end

  test "18 day pass" do
    result = create_item() |> elapse_days(18)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -5, quality: 0},
      %Item{name: @brie, sell_in: -5, quality: 24},
    ]
  end

  test "19 day pass" do
    result = create_item() |> elapse_days(19)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -6, quality: 0},
      %Item{name: @brie, sell_in: -6, quality: 26},
    ]
  end

  test "20 day pass" do
    result = create_item() |> elapse_days(20)
    assert result === [
      %Item{name: @sulfuras, sell_in: 13, quality: 1},
      %Item{name: @concert_ticket, sell_in: -7, quality: 0},  
      %Item{name: @brie, sell_in: -7, quality: 28},
    ]
  end

end
