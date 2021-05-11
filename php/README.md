# GildedRose Kata - PHP Version

See the [top level readme](../README.md) for general information about this exercise. This is the PHP version of the
GildedRose Kata.

## Installation

The kata uses:

- [PHP 8.0+](https://www.php.net/downloads.php)
- [Composer](https://getcomposer.org)
- [Git](https://git-scm.com/downloads)


## Dependencies

The project used composer to install:

- [PHPUnit](https://phpunit.de/)
- [ApprovalTests.PHP](https://github.com/approvals/ApprovalTests.php)
- [PHPStan](https://github.com/phpstan/phpstan)
- [Easy Coding Standard (ECS)](https://github.com/symplify/easy-coding-standard)
- [PHP CodeSniffer](https://github.com/squizlabs/PHP_CodeSniffer/wiki)

## Folders

- `src` - contains the two classes:
    - `Item.php` - this class was not changed;
    - `GildedRose.php` - this class was refactored, and the new feature added;
- `tests` - contains the tests:
    - `GildedRoseTest.php` - starter test.
- `Fixture`
    - `texttest_fixture.php` this was used by an ApprovalTests, and run from the command line

## Testing

Tests were written for all the requirement specifications listed in ["Gilded Rose Requirements"](https://github.com/emilybache/GildedRose-Refactoring-Kata/tree/master/GildedRoseRequirements.txt).

Test methods takes initial (starting) values, that can be changed accordingly. E.g., "31" for sell_in value and "50" for quality value:

```shell
$items = [new Item('+5 Dexterity Vest', 31, 50)];
```
puts them in GildedRose Class updateQuality() method and checks for expected values. E.g., "30" for sell_in value and "49" for quality value:

```shell
self::assertSame(30, $items[0]->sell_in);
self::assertSame(49, $items[0]->quality);
```

Furthermore, where more than one arbitrary arguments are desired for single test, @dataProvider method was used and relevant Scenarios were created. E.g., 'data' array accepts initial values for sell_in and quality and 'expect' array - expected ones.

```shell
public function qualityNeverNegativeScenario(): array
    {
        return [
            [[
                'data' => [
                    'sell_in' => 1,
                    'quality' => 0,
                ],
                'expect' => [
                    'sell_in' => 0,
                    'quality' => 0,
                    
                ]
            ]]
```

To run the unit tests, from the root of the PHP
project run:

```shell script
composer test
```

A Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias pu="composer test"`), the same
PHPUnit `composer test` can be run:

```shell script
pu
```

### Approval Test

'sell_in' and 'quality' values of GildedRose items for 31 day were generated via terminal command from the root of the PHP project:

```shell
php fixtures/texttest_fixture.php
```
and compared to provided ["Approval Test document"](tests/approvals/ApprovalTest.testTestFixture.approved.txt) until data were identical.
### Tests with Coverage Report

To run all test and generate a html coverage report run:

```shell script
composer test-coverage
```

The test-coverage report will be created in /builds, it is best viewed by opening /builds/**index.html** in your
browser.

## Code Standard

Easy Coding Standard (ECS) is configured for style and code standards, **PSR-12** is used. The current code is not upto
standard!

### Check Code

To check code, but not fix errors:

```shell script
composer check-cs
``` 

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias cc="composer check-cs"`), the same
PHPUnit `composer check-cs` can be run:

```shell script
cc
```

### Fix Code

ECS provides may code fixes, automatically, if advised to run --fix, the following script can be run:

```shell script
composer fix-cs
```

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias fc="composer fix-cs"`), the same
PHPUnit `composer fix-cs` can be run:

```shell script
fc
```

## Static Analysis

PHPStan is used to run static analysis checks:

```shell script
composer phpstan
```

On Windows a batch file has been created, like an alias on Linux/Mac (e.g. `alias ps="composer phpstan"`), the same
PHPUnit `composer phpstan` can be run:

```shell script
ps
```


