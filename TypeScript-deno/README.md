# Gilded Rose

This is the Gilded Rose kata in TypeScript through [Deno](https://deno.com/) for a simplified experience.

## Getting started

Install dependencies

No more dependencies to install. Just go to the next step.

## Run the unit tests from the Command-Line

Open a terminal and run the following command:

```sh
deno test
```

To run all tests in watch mode

```sh
deno test --watch
```

## Run the TextTest fixture from the Command-Line

```sh
deno run test/golden-master-text-test.ts
```

Or with number of days as args:
```sh
deno run test/golden-master-text-test.ts 10
```

You should make sure the command shown above works when you execute it in a terminal before trying to use TextTest (see below).


## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest. You will need to specify the Python executable and interpreter in [config.gr](../texttests/config.gr). Uncomment these lines:

    executable:${TEXTTEST_HOME}/python/texttest_fixture.py
    interpreter:python
