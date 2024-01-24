Feature: Backstage passes quality changes
  In order to keep track of backstage passes
  As a shopkeeper
  I want to see the quality of backstage passes change appropriately


  Scenario: Backstage pass quality update
    Given a backstage pass with a sell-in of 20 and a quality of 20
    When I update the quality
    Then the item should have a quality of 21


  Scenario: Backstage pass with 10 days left
    Given a backstage pass with a sell-in of 10 and a quality of 20
    When I update the quality
    Then the item should have a quality of 22


  Scenario: Backstage pass with 5 days left
    Given a backstage pass with a sell-in of 5 and a quality of 20
    When I update the quality
    Then the item should have a quality of 23


  Scenario: Backstage pass after the concert
    Given a backstage pass with a sell-in of 0 and a quality of 20
    When I update the quality
    Then the item should have a quality of 0
