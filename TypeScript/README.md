# Gilded Rose

This is the Gilded Rose kata in TypeScript.

## Architecture

The update logic is encapsulated in behaviors that define how different types of items are updated.

- The `Item` class represents an item with properties like `name`, `sellIn`, and `quality`.

- The `IUpdateBehavior` interface defines the contract for item update behaviors. Which is basically an `update` method that takes an `Item` and returns an updated `Item`. Each specific item type has its own implementation of this interface, such as `AgedBrieBehavior`, `BackstagePassBehavior`, etc. You can find these implementations in the `app/update-behaviors/implementations` directory.

- The `getUpdateBehaviorFor(item)` function checks the item name and returns the correct instantiated behavior class. This is done in the `app/update-behaviors/behavior-resolver.ts` file.

- The `GildedRose` class is responsible for managing the items. It has an `updateQuality` method that iterates through the items, retrieves the appropriate update behavior for each item via the resolver, and calls its `update` method.

## Getting started

Install dependencies

```sh
npm install
```

## Run the unit tests from the Command-Line

I chose to use Jest for unit tests and snapshot tests.

```sh
npm run test
```

To run all tests in watch mode

```sh
npm run test:watch
```
