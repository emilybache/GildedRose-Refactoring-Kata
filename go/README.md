# GO Starter

- Run :

```shell
go run texttest_fixture.go gilded-rose.go
```

- Run tests :

```shell
go test
```

- Run tests and coverage :

```shell
go test -coverprofile=coverage.out

go tool cover -html=coverage.out
```

- Create golden test file using textest (*nix):

```bash
go run texttest_fixture.go gilded-rose.go > golden_test.txt 
```

- Testing changes against the golden test file (*nix):

```bash
diff <(go run texttest_fixture.go gilded-rose.go) golden_test.txt
```
