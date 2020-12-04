# Gilded Rose

## The task

Take existing legacy code and add a new feature.

## Approach

1. Write tests for existing functionality.  
> This allowed me to refactor the code while knowing I wasn't breaking any functionality.
2. Refactor existing code incrementally.
> It is valuable to have the code formatted using best practices. In order not to break existing functionality I refactored incrementally, running tests and commiting after each small change. This refactor made it much easier to add my feature, but will also improve long term reliability of the code, and future proof it for future feature additions.
3. TDD new feature.
> Having performed the previous two steps, this one was very easy.

## Limitations

The test spec required that I did not alter the Item class. This made it more difficult to apply OOD principles. I have instead created a module for each item type, which still allows for encapsulation.  

My tests verged on state driven rather than behaviour driven. I could not find an equivalent of RSpec's "change by" matcher for Jasmine. Any tips would be appreciated.

With more time I would add a 'sellInChange' function to each item type to mirror the qualityChange. The different sellIn change for Sulfurus is still handled by exception.

## Tests

To run the tests and see code coverage run 

```
git clone https://github.com/dan-holmes/gilded-rose.git
cd GildedRose-Refactoring-Kata
cd js-jasmine
npm install
npm run test
```

Current test coverage:
![test coverage](https://i.imgur.com/vDJgGqL.png)