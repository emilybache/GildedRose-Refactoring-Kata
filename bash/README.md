# Requirements

`bash` and friends (`diff`, `grep`, `cat`)

# (Failing) Unit Test

```shell
./unit_test.sh
```

# Texttest Fixture

```shell
./texttest_fixture.sh
```

Specify days:

```shell
./texttest_fixture.sh 30
```

Verify againt `ThirtyDays/stdout.gr`

```shell
./verify.sh
```

## BTW

BTW, the script is a pure "function", so this works:

```shell
$ echo -e 'Aged Brie|3|5\nOther Item|4|5' |
> ./gilded_rose.sh |
> ./gilded_rose.sh |
> ./gilded_rose.sh
Aged Brie|0|8
Other Item|1|2
```
