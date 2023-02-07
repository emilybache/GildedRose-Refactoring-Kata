# Installs

[install jaq](https://github.com/01mf02/jaq#installation) (latest development version)

# Failing Unit Test

```shell
./test-gilded-rose.sh
```

# TextTest Fixture

```shell
jaq -nr "$(cat gilded-rose.jq) $(cat texttest_fixture.jq)"
```

Specify days (e.g. 10 days):

```shell
jaq --arg days 10 -nr "$(cat gilded-rose.jq) $(cat texttest_fixture.jq)"
```
