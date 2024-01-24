Feature: Brie quality changes
  In order to keep track of aged brie
  As a shopkeeper
  I want to see the quality of aged brie increase


  Scenario: Aged brie quality update
    Given some brie with a sell-in of 20 and a quality of 20
    When I update the quality
    Then the item should have a quality of 21


  Scenario: Aged brie at maximum quality
    Given some brie with a sell-in of 20 and a quality of 50
    When I update the quality
    Then the item should have a quality of 50

  Scenario: Aged brie gets better faster after sell-by date
    Given some brie with a sell-in of 0 and a quality of 20
    When I update the quality
    Then the item should have a quality of 22
