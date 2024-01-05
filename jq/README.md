# Requirements

jq

# Failing Unit Test

```shell
./test-gilded-rose.sh
```

# TextTest Fixture

```shell
jq -nr "$(cat gilded-rose.jq) $(cat texttest_fixture.jq)"
```

Specify days (e.g. 10 days):

```shell
jq --arg days 10 -nr "$(cat gilded-rose.jq) $(cat texttest_fixture.jq)"
```
