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
The different derived classes will be moved to their own files in the future. Left them together for now to show the steps.
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
14. Added the new Conjured items requirements. Due to the previous refactorings above, I only needed to change the following files/classes:
  - `ItemType` to add the new type enum and definition
  - Add a new `DailyUpdaterForConjuredItems` updater to handle the new type's requirements
  - `DailyUpdaterFactory` to create and return the new updater<br>
Note that I didn't need to change the client's code (GildedRose) or any of the other items types code.


## Final solution
- Used `Template` pattern in `DailyUpdater` hierarcy to support the general `DailyUpdate` template and allow each derived class (per item type) to implement their own `UpdateQuality` algorithm.
  - _Why_: instead of using a lot of if-else statements, every item type implement its own unique processing and the shared processing exist in the base Template class. When adding new item types, we can just add a new updater class without having to change the Template or any of the other updaters' code
- Used a `Simple Factory` to rturn the correct Updater for a given item (type).
  - _Why_: Move the knowledge about the different types of items away from the client code (GildedRose.cs) so it only relies on the abstract factory to return the correct updater for every item. When adding new item types, the client code will not have to change.
- Used `semi-Flyweight` pattern to re-use the existing updaters instead of creating new ones over and over again. Note: In this instance, we could have used a dependency injection framework instead.
  - _Why_: this one in not critical. However it seemed wasteful to create new updaters over and over again when they don't have any unique (extrinsic) properties and can use just one updater of each type instead.
- Used a `Builder` pattern to enforce building valid items.
  - _Why_: to prevent building items the violate the requirement. Specifically to enforce the quality limits and the legacy items special quality value.


## Suggested Improvements if we could change anything
The used solution is good in general, I would probably do the following changes as well:
- Define `Quality` as a TinyType that protects its limits and derive `LegendaryQuality` class to support the legendary items special quality rules.
- Add properties to Item:
  - Type - use enum to define the type. This will allow better seperation between the name of the item and its type and will make the rest of our processing easier and more readable.
  - Updater - a rference to the item's updater that can be created in the constructor depending on the item's type (usign the simple factory). From then on, this can be called directly on the item. This will use a `semi-Strategy` pattern in combination with the `Template` pattern in that the updater behaviour is being plugged into the objects.
- the `Builder` might be redundant now that the items can enforce their own valid creation. 
- Instead of using the `Flywieght` pattern, I would probably use a dependency injection framework to manage the creation of the updaters.