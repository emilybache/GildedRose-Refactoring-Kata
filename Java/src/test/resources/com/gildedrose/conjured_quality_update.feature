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