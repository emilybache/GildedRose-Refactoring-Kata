defmodule GildedRoseTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "Approval test 30 days" do
    expected = File.read!("test/ApprovalTest.ThirtyDays.verified.txt") |> String.replace("\r\n", "\n")
    result = capture_io(fn -> GildedRose.TextTestFixture.run(30) end) |> String.replace("\r\n", "\n")
    assert result == expected
  end

  test "begin the journey of refactoring" do
    items = [%Item{name: "foo", sell_in: 0, quality: 0}]
    GildedRose.update_quality(items)
    %{name: firstItemName} = List.first(items)
    assert "fixme" == firstItemName
  end
end
