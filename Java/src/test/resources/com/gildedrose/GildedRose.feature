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

