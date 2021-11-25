# Gilded Rose Kata in Java

Forked version of the Gilded-Rose-Refactoring-Kate project, original can be found
[here](https://github.com/emilybache/GildedRose-Refactoring-Kata).

# Notes

## Item rules

* Quality increases or decreases when sell-in days pass or is constant.
* Quality of an item cannot be negative value.
* If item is Legendary, it's quality is always 80 and never changes.

## Types of items

| Item name | Quality changes, when day passes |  Description |
|:----------|:---------------------------------|:------------- |
| Normal | **Quality decreases by 1;** <br> **Quality decreases by 2**, when sell-in date approaches | - |
| Aged Brie | **Quality increases by 1;** <br> **Quality increases by 2**, when sell-in date approaches | - |
| Conjured | <b>Quality decreases by 1;</b> <br> <b>Quality decreases by 4</b> when sell-in date approaches | - |
| Backstage Pass | <b>Quality increases by 1;</b><br> <b>Quality increases by 2</b> when there are 10 days or less;<br> <b>Quality increases by 3 </b> when there are 5 days or less;<br> <b>Quality drops to 0</b> after the concert. | - |
| Sulfura | **Always 80**. Never changes| Legendary item. Constant quality value. |
