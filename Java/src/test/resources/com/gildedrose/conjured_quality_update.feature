Feature: Conjured items quality update

  Scenario Outline: Conjured items quality degrades by two each day
    Given name is "<name>", quality is "<quality>" and sellIn is "<sellIn>"
    When I calculateQuality
    Then I should have new quality "<newQuality>" and new sellIn "<newSellIn>"

    Examples:
      | name          | quality | sellIn | newQuality | newSellIn |
      | Conjured      | 8       | 10     | 6          | 9         |
      | Conjured      | 3       | 2      | 1          | 1         |
      | Conjured      | 1       | 1      | 0          | 0         |
      | Conjured      | 6       | 0      | 2          | -1        |
      | Conjured      | 4       | 0      | 0          | -1        |


  Scenario Outline: Standard items quality degrades by 1 each day,
  but degrade by 2 when sellIn is negative
    Given name is "<name>", quality is "<quality>" and sellIn is "<sellIn>"
    When I calculateQuality
    Then I should have new quality "<newQuality>" and new sellIn "<newSellIn>"

    Examples:
      | name         | quality | sellIn  | newQuality | newSellIn |
      | foo          | 0       | -1      | 0          | -2        |
      | foo          | 1       | -1      | 0          | -2        |
      | foo          | 49      | -1      | 47         | -2        |
      | foo          | 50      | -1      | 48         | -2        |
      | foo          | 0       | 0       | 0          | -1        |
      | foo          | 1       | 0       | 0          | -1        |
      | foo          | 49      | 0       | 47         | -1        |
      | foo          | 50      | 0       | 48         | -1        |
      | foo          | 0       | 5       | 0          | 4         |
      | foo          | 1       | 5       | 0          | 4         |
      | foo          | 49      | 5       | 48         | 4         |
      | foo          | 50      | 5       | 49         | 4         |
      | foo          | 0       | 6       | 0          | 5         |
      | foo          | 1       | 6       | 0          | 5         |
      | foo          | 49      | 6       | 48         | 5         |
      | foo          | 50      | 6       | 49         | 5         |
      | foo          | 0       | 11      | 0          | 10        |
      | foo          | 1       | 11      | 0          | 10        |
      | foo          | 49      | 11      | 48         | 10        |
      | foo          | 50      | 11      | 49         | 10        |