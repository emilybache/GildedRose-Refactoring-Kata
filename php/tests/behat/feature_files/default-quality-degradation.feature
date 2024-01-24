Feature: Default quality degradation
  In order to keep track of item quality
  As a shopkeeper
  I want to see the quality of an item decrease by 1 each day


  Scenario: Single quality update
    Given an item with a sell_in of 20 and a quality of 20
    When I update the quality
    Then the item should have a quality of 19


  Scenario: Quality updates over multiple days
    Given an item with a sell_in of 20 and a quality of 20
    When I update the quality 5 times
    Then the item should have a quality of 15
