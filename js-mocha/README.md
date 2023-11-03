# Gilded Rose in Javascript with Mocha

## Getting started

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
