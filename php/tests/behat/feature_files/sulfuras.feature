Feature: Sulfuras
  In order to keep track of sulfuras
  As a shopkeeper
  I want to see the quality and sell-in of sulfuras stay the same


  Scenario: Sulfuras quality and sell-in not changing 
    Given a sulfura with a sell-in of 20 and a quality of 20
    When I update the quality
    Then the item should have a quality of 20
    And the item should have a sell-in of 20
