Feature: Gilded Rose quality
  I want to know if the quality is updated properly

  Scenario: Check quality degradation for normal article
    Given For article "foo" with initial quality 10 and sellIn 5
    When The quality is updated the next 10 days
    Then I should get the following quality values each day:
      | day | quality | sellIn |
      | 1   | 9       | 4      |
      | 2   | 8       | 3      |
      | 3   | 7       | 2      |
      | 4   | 6       | 1      |
      | 5   | 5       | 0      |
      | 6   | 3       | -1     |
      | 7   | 1       | -2     |
      | 8   | 0       | -3     |
      | 9   | 0       | -4     |
      | 10  | 0       | -5     |

Scenario: Check quality inceases for Aged Brie
    Given For article "Aged Brie" with initial quality 45 and sellIn 5
    When The quality is updated the next 10 days
    Then I should get the following quality values each day:
        | day | quality | sellIn |
        | 1   | 46       | 4      |
        | 2   | 47       | 3      |
        | 3   | 48       | 2      |
        | 4   | 49       | 1      |
        | 5   | 50       | 0      |
        | 6   | 50       | -1     |
        | 7   | 50       | -2     |
        | 8   | 50       | -3     |
        | 9   | 50       | -4     |
        | 10  | 50       | -5     |

    Scenario: Check Sulfuras has a  quality of 80 and it never alters
        Given For article "Sulfuras, Hand of Ragnaros" with initial quality 80 and sellIn 5
        When The quality is updated the next 10 days
        Then I should get the following quality values each day:
            | day | quality | sellIn |
            | 1   | 80       | 5      |
            | 2   | 80       | 5      |
            | 3   | 80       | 5      |
            | 4   | 80       | 5      |
            | 5   | 80       | 5      |
            | 6   | 80       | 5     |
            | 7   | 80       | 5     |
            | 8   | 80       | 5     |
            | 9   | 80       | 5     |
            | 10  | 80       | 5     |

Scenario: Check Backstage passes increased in quality by one when sellIn is more than 10, by two when sellIn is 10 or less and by three when sellIn is 5 or less. Quality drops to 0 after the concert
    Given For article "Backstage passes to a TAFKAL80ETC concert" with initial quality 22 and sellIn 15
    When The quality is updated the next 17 days
    Then I should get the following quality values each day:
        | day | quality | sellIn |
        | 1   | 23       | 14      |
        | 2   | 24       | 13      |
        | 3   | 25       | 12      |
        | 4   | 26       | 11      |
        | 5   | 27       | 10      |
        | 6   | 29       | 9      |
        | 7   | 31       | 8      |
        | 8   | 33       | 7      |
        | 9   | 35       | 6      |
        | 10  | 37       | 5      |
        | 11  | 40       | 4      |
        | 12  | 43       | 3      |
        | 13  | 46       | 2      |
        | 14  | 49       | 1      |
        | 15  | 50       | 0      |
        | 16  | 0       | -1      |
        | 17  | 0       | -2      |

    Scenario: Check Backstage passes increased in quality by one when sellIn is more than 10, by two when sellIn is 10 or less and by three when sellIn is 5 or less. Quality drops to 0 after the concert
        Given For article "Backstage passes to a TAFKAL80ETC concert" with initial quality 45 and sellIn 15
        When The quality is updated the next 17 days
        Then I should get the following quality values each day:
            | day | quality | sellIn |
            | 1   | 46       | 14      |
            | 2   | 47       | 13      |
            | 3   | 48       | 12      |
            | 4   | 49       | 11      |
            | 5   | 50       | 10      |
            | 6   | 50       | 9      |
            | 7   | 50       | 8      |
            | 8   | 50       | 7      |
            | 9   | 50       | 6      |
            | 10  | 50       | 5      |
            | 11  | 50       | 4      |
            | 12  | 50       | 3      |
            | 13  | 50       | 2      |
            | 14  | 50       | 1      |
            | 15  | 50       | 0      |
            | 16  | 0       | -1      |
            | 17  | 0       | -2      |

    Scenario: Check Conjured items degrade in quality twice as fast as normal items
        Given For article "Conjured Mana Cake" with initial quality 10 and sellIn 5
        When The quality is updated the next 10 days
        Then I should get the following quality values each day:
            | day | quality | sellIn |
            | 1   | 8       | 4      |
            | 2   | 6       | 3      |
            | 3   | 4       | 2      |
            | 4   | 2       | 1      |
            | 5   | 0       | 0      |
            | 6   | 0       | -1     |
            | 7   | 0       | -2     |
            | 8   | 0       | -3     |
            | 9   | 0       | -4     |
            | 10  | 0       | -5     |
