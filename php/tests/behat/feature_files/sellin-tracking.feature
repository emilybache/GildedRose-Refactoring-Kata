Feature: Sell-in tracking
  In order to keep track of the sell-by date of an item
  As a shopkeeper
  I want to see the sell-in value decremented each day


  Scenario: Basic sell-in tracking
    Given an item with a sell-in of 20
    When I update the sell-in
    Then the item should have a sell-in of 19
