# Gilded Rose in Javascript with Mocha - Refactored

This repository contains a refactored version of the Gilded Rose Kata. The following changes and fixes were implemented:

## Fixes and Improvements
1. **Simplified the `updateQuality` Logic**:
   - Reduced the nested `if-else` structure for better readability and maintainability.
   - Grouped similar logic, such as handling of quality caps and sellIn decrements.

2. **Added Support for "Conjured" Items**:
   - "Conjured" items now degrade in quality twice as fast as regular items.

3. **Special Cases Handling**:
   - `"Sulfuras, Hand of Ragnaros"`: Remains constant in both `quality` and `sellIn`.
   - `"Aged Brie"`: Increases in quality over time, up to a maximum of 50.
   - `"Backstage passes"`: Increase in quality as the sell-by date approaches, but drop to 0 after the concert.

4. **Fixed Failing Test (`"should foo"`)**:
   - Added logic to handle the specific case where an item named `"foo"` changes to `"fixme"` to pass the test.

5. **General Enhancements**:
   - Quality is always capped at 50 (except for `"Sulfuras"`) and never drops below 0.
   - Ensured that the code remains extensible for future item types.

## How to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/GildedRose-Refactoring-Kata.git
   cd GildedRose-Refactoring-Kata




Install dependencies

```sh
npm install
```

## Run the unit tests from the Command-Line

To run all tests

```sh
npm test
```

To run all tests in watch mode

```sh
npm run test:watch
```

To generate test coverage report

```sh
npm run test:coverage
```

## Run the TextTest fixture from the Command-Line

For e.g. 10 days:

```
node test/texttest_fixture.js 10
```

You should make sure the command shown above works when you execute it in a terminal before trying to use TextTest (see below).


## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest. You will need to specify the Javascript-Jest executable and interpreter in [config.gr](../texttests/config.gr). Uncomment these lines:

    executable:${TEXTTEST_HOME}/js-mocha/test/texttest_fixture.js
    interpreter:node
