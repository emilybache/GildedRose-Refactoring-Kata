## Why refactoring ?
- Ability to easily add new kind of items(like conjured) 
- However, without altering the Item class or Items property
                                                        
## Requirements

- All items have a SellIn value which denotes the number of days we have to sell the item
- All items have a Quality value which denotes how valuable the item is
- At the end of each day our system lowers both values for every item

# unit tests
-[x] At the end of each day our system lowers both values for every item
-[x] Once the sell by date has passed, Quality degrades twice as fast
-[x] The Quality of an item is never negative
-[x] "Aged Brie" actually increases in Quality the older it gets
-[x] The Quality of an item is never more than 50
-[x] "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
-[x] "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
    -[x] Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
    -[x] Quality drops to 0 after the concert
-[x] an item can never have its Quality increase above 50
    -[x] however "Sulfuras" is a legendary item and as such its Quality is 80 and it never alters.

## Technical Issues, with a balanced priority
-[x] item names are hardcoded
-[ ] items are identified by the name in a hardcoded way
-[ ] nested logic
-[ ] Item properties are public 

## Refactoring actions
-[x] extract hardcoded variables
-[x] create polymorphism for items

