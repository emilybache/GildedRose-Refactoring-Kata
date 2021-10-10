Added cucumber for running the tests and 
added the lines written in the specification as is in this. 

The cucumber feature file example is somewhat like:

**src/test/resources/GildedRose.feature**
```gherkin
Feature: Gilded Rose quality
  I want to know if the quality is updated properly

  Scenario: Sellin and Quality decreasing
    Given for "+5 Dexterity Vest" initial sellin is 20 and quality is 30
    When I update the quality
    Then I should get sellin as 19 and quality as 29
  ...
```

And the result is shown as:

```
$ ./gradlew cucumber

> Task :cucumber

Scenario: Sellin and Quality decreasing                                # src/test/resources/GildedRose.feature:4
  Given for "+5 Dexterity Vest" initial sellin is 20 and quality is 30 # com.gildedrose.StepDefinitions.initial_sellin_is_and_quality_is(java.lang.String,int,int)
  When I update the quality                                            # com.gildedrose.StepDefinitions.i_update_the_quality()
  Then I should get sellin as 19 and quality as 29                     # com.gildedrose.StepDefinitions.i_should_get_sellin_as_and_quality_as(int,int)

1 Scenarios (1 passed)
3 Steps (3 passed)
0m0.376s



BUILD SUCCESSFUL in 2s
```

Note: If you want to decrease update the quality twice, just repeat the line "When I update the quality" twice in the feature file.
