# Tips and Tricks

## Run test:

```shell
dotnet test
```

## Run test whenever a files are updated (saved):

```shell
dotnet watch --project GildedRoseTests test
```

## Run test and collect code coverage:

```shell
rm -rf GildedRoseTests/TestResults/* && dotnet test --collect:"XPlat Code Coverage"
```

> A file named `coverage.cobertura.xml` will be created for each run. It can be parsed by (e.g) VS Code's [Coverage Gutters](https://marketplace.visualstudio.com/items?itemName=ryanluker.vscode-coverage-gutters) extension.
