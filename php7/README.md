Requirements
------------

**PHP 7:**

This is usually bundled with your operating system, or fetchable using a package manager like `apt` or `homebrew`.

Windows users can find the latest version here: https://windows.php.net/download#php-7.3

If you want to compile from source code, that can be found here: https://www.php.net/downloads.php

**Composer:**

Composer is PHP's main package and dependency management tool.

It can be downloaded here: https://getcomposer.org/download/

Getting Started
---------------

To begin the kata, install the dependencies and run `phpunit`:

```
cd php7
composer install
vendor/bin/phpunit
```

If the "install" command does not work, try running `composer update` instead.
This will tell composer that it has permission to look for a newer version of
its dependencies.

If things are still not cooperating, you can try this extreme approach:

```
composer remove phpunit/phpunit
composer require phpunit/phpunit
```

To exercise the code outside of phpunit, for example to visually confirm that it is working,
use the `texttest_fixture` script:

```
php fixtures/texttest_fixture.php
```

Tips
----

PHPUnit has a very thorough reference manual. It would be particularly useful to explore the
[Data Providers](https://phpunit.readthedocs.io/en/8.1/writing-tests-for-phpunit.html#data-providers) section.
