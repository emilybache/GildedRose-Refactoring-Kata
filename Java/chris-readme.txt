ItemHolder is an abstract class that implements the update template (first update quality then sellIn).
Concrete classes implement algorithm for each item type.
ItemHolder object creation is put into its own factory class as it will change with new item types.

Improvements:
Test code is highly duplicated. I assume Junit has paramaterized tests, just didn't bother looking it up.
ItemHolders do checks for max and min values. Maybe those belong in setSellIn/setQuality methods?
