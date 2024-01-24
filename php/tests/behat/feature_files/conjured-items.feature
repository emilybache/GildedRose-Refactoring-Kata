Feature: Conjured items
  In order to keep track of conjured items
  As a shopkeeper
  I want to see the quality of a conjured item decrease by 2 each day


  Scenario: Single quality update
    Given a conjured item with a sell-in of 20 and a quality of 20
    When I update the quality
    Then the item should have a quality of 18


  Scenario: Quality cannot be negative
      Given a conjured item with a sell-in of 20 and a quality of 0
      When I update the quality
      Then the item should have a quality of 0


  Scenario: Once the sell-by date has passed, quality degrades twice as fast
      Given a conjured item with a sell-in of 0 and a quality of 20
      When I update the quality
      Then the item should have a quality of 16
