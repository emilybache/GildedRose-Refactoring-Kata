<p align="center">
  <img src="https://avatars.githubusercontent.com/u/5228734?s=280&v=4" alt="NFQ Group"/>
</p>

# GildedRose Kata - PHP Version

## Installation

##### The kata uses:

- [8.0+](https://www.php.net/downloads.php)
- [Composer](https://getcomposer.org)

##### Recommended:

- [Git](https://git-scm.com/downloads)

See [GitHub cloning a repository](https://help.github.com/en/articles/cloning-a-repository) for details on how to
create a local copy of this project on your computer.

```sh
git clone git@github.com:thang-tran-nfq/GildedRose-Refactoring-Kata.git
```

or

```shell script
git clone https://github.com/thang-tran-nfq/GildedRose-Refactoring-Kata.git
```

##### Setup docker-compose
```shell script
docker-compose up -d
```

##### Exec the docker container
```shell script
docker exec -it php-cli bash 
```

##### Install all the dependencies using composer

```shell script
cd ./GildedRose-Refactoring-Kata
composer install
```

## Dependencies

##### The project uses composer to install:

- [PHPUnit](https://phpunit.de/)
- [ApprovalTests.PHP](https://github.com/approvals/ApprovalTests.php)
- [PHPStan](https://github.com/phpstan/phpstan)
- [Easy Coding Standard (ECS)](https://github.com/symplify/easy-coding-standard)

## Folders

- `src` - contains the two classes:
    - `Item.php` - this class should not be changed
    - `GildedRose.php` - this class needs to be refactored, and the new feature added
    - `Items` - This folder contains all Items Class
      - `Abstract` - This folder store Abstract of all Item Class
      - `Interface` - This folder store Interface of all Item Class
      - `Factory` - This folder store Factory, create Item instance
      - `AgedBrieItem.php` 
      - `BackstagePassItem.php`
      - `ConjuredItem.php`
      - `NormalItem.php`
      - `SulfurasItem.php`
- `tests` - contains the tests
    - `GildedRoseTest.php` - starter test.
        - Tip: ApprovalTests has been included as a dev dependency, see the PHP version of
          the [Theatrical Players Refactoring Kata](https://github.com/emilybache/Theatrical-Players-Refactoring-Kata/)
          for an example
- `Fixture`
    - `texttest_fixture.php` this could be used by an ApprovalTests, or run from the command line

## Fixture

##### To run the fixture from the php directory:

```shell
php .\fixtures\texttest_fixture.php 10
```

Change **10** to the required days.

## Testing

PHPUnit is configured for testing, a composer script has been provided. To run the unit tests, from the root of the PHP
project run:

```shell script
composer tests
```

### Tests with Coverage Report

To run all test and generate a html coverage report run:

```shell script
composer test-coverage
```

The test-coverage report will be created in /builds, it is best viewed by opening /builds/**index.html** in your
browser.

The [XDEbug](https://xdebug.org/download) extension is required for generating the coverage report.

## Code Standard

Easy Coding Standard (ECS) is configured for style and code standards, **PSR-12** is used. The current code is not upto
standard!

### Check Code

To check code, but not fix errors:

```shell script
composer check-cs
``` 

### Fix Code

ECS provides may code fixes, automatically, if advised to run --fix, the following script can be run:

```shell script
composer fix-cs
```

## Static Analysis

PHPStan is used to run static analysis checks:

```shell script
composer phpstan
```

**Happy coding**!
test 1