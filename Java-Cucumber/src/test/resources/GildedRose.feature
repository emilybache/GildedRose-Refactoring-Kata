Feature: Gilded Rose quality
  I want to know if the quality is updated properly

  Scenario: Sellin and Quality decreasing
    Given for "+5 Dexterity Vest" initial sellin is 20 and quality is 30
    When I update the quality
    Then I should get sellin as 19 and quality as 29
