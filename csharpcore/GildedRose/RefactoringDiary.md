This file documents my refactoring and design journey while working through in this kata.

NOTES:
- At this stage, I assume that when we're not supposed to change Item or Items, we shouldn't just ignore them and create a parallel object types to use (that include the different DailyUpdate behaviours).
- This code and requirements were simple enough that I could have created the final version, or at least a massive refactoring immediately.
  However, I wanted to experience and demonstrate the refactoring process by small steps that can be just as easily applied to more complex code.


## My refactoring steps:
1. Added full test coverage drawing from the requirements and the existing 30 days texttest test.<br>
Some of the tests failed due to bugs I discovered in the code/existing tests (e.g. relying on exact name instead of the mentioned "code word"). 
I decorated these tests with `[Ignore]` until I get to a stage in the refactoring where I can easily fix the issues and re-introduce the tests.   
2. small refactorings of the UpdateQuality() method to make it more readable and separate the different (unrelated) handling of the different types.<br>
3. Extract Increasing/Decreasing Quality into a method.<br>  
4. Separated the processing of the different types from each other.<br>
5. Used the `Template` design pattern to create DailyUpdater abstract class and derived classes to handle the DailyUpdate's steps for the different types of items.<br>
The different derived classes will be moved to their own files in the next PR. Left them together for now to show the steps.
6. Created a `Simple Factory` method to return the appropriate DailyUpdater object for the item type.<br>
7. Moved DailyUpdaterFactory to its own class.
8. Moved the logic of "Once the sell by date has passed, Quality degrades twice as fast" into the DailyUpdater Template class since it's relevant to all types of items.
9. Moved Decrease/IncreaseItem byValue to be 1 by default.
10. Refactored DailyUpdaterFactory to reuse existing Updaters instead of creating new ones every time.<br>
This could have been done using a dependency injection framework as well (to automatically create and reuse all the updaters). This might actually be the better way to do this. 
However, I've chosen to use the factory itself to manually manage it using a semi-flyweight pattern.
11. Moved ItemType and ItemQuality to their own classes to allow re-use
12. Seperated the derived DailyUpdater classes into their own files.<br>
Now, adding a new updater won't have to change the existing DailyUpdater file.
13. Added a new ItemBuilders hierarchy to enforce building valid items (quality limits and special rules for Legendary items). Also added unit tests and refactored the existing tests to use the new builders.<br>
note: if we could change Item, this could have been implemented in the Item's constructor itself and by using Quality TinyType. However, since we can't change Item, the builders gives us a nice decoupled way to validate the items are constructed consistently.  



This is the state of the code now


### Considered but dropped
This is all the design ideas/patterns I've considered but decided against
- It feels like Quality should have been a `TinyType` that manages its own limits. However, since we're not allowed to change Item I'm not implementing this.
- 


