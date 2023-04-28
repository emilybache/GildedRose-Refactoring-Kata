# Exercise Documentation

This documentation serves the purpose to follow my train of thought in doing this exercise. 
I will split the exercise into roughly 3 parts:

- Add more tests to the exercise
- refactor the code
- implement the new feature


## Tests

we can add more tests using the criteria listed in the description of the exercise:

- Once the sell by date has passed, Quality degrades twice as fast
- The Quality of an item is never negative
- “Aged Brie” actually increases in Quality the older it gets
- The Quality of an item is never more than 50
- “Sulfuras”, being a legendary item, never has to be sold or decreases in Quality
- “Backstage passes”, like aged brie, increases in Quality as it’s SellIn value approaches; Quality increases by 2 when 
- there are 10 days or less and by 3 when there are 5 days or less but Quality drops to 0 after the concert

This will increase our chances we keep the original functionality working as is when refactoring. 

## Refactor

The plan is to refactor the updateQuality method and split out the above criteria for each different item. This will 
make it easier to then implement an extended Item class to which we can delegate updating of the quality of the object.

Because the updateQuality method is pretty cumbersome, it's probably easier to split it into smaller chunks first before
creating separate classes for the objects.

## Implement new feature

Once we have finished with the, the new feature can be implemented in its own separate class, and for this and future 
new items, we don't have to modify the core code anymore.