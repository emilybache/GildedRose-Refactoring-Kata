# Gilded Rose starting position in Ruby

## Installation


## Run the unit tests from the Command-Line

Ensure you have RSpec installed

    gem install rspec

```
rspec gilded_rose_spec.rb
```

## Run the TextTest fixture from the Command-Line

For e.g. 10 days:

```
ruby texttest_fixture.rb 10
```

You should make sure the command shown above works when you execute it in a terminal before trying to use TextTest (see below).

## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest. You will need to specify the Ruby executable and interpreter in [config.gr](../texttests/config.gr). Uncomment these lines:

    executable:${TEXTTEST_HOME}/ruby/texttest_fixture.rb
    interpreter:ruby
