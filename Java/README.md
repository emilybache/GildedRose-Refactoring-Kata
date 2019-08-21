## Gilded Rose Kata - Java

`$ mvn clean compile`

`$ mvn clean verify`

### How to add a new updater
Within this version of Gilded Rose kata, updateQuality() method is refactored by using template method pattern.

From now on it is easy to add another **standard**, **custom** or **legendary** item to the shop.
How to do that;

* Add a new Updater class which extends either `StandardItemUpdater`, `CustomItemUpdater` or `LegendaryItemUpdater`
* Implement required methods as your requirement. 
* Add a new registry to `registeredItemUpdaters` via `ItemUpdaterFactory.registerCustomUpdater()`
* You are ready to call `updateQuality()` method of a `GildedRose` instance.
