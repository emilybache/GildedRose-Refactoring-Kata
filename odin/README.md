# Gilded Rose starting position in Odin

Learn how to install Odin on your system [here](https://odin-lang.org/docs/install/)

## Run unit tests from the command line
```
$ odin test tests
```
## Run the TextTest fixture on the command line
Build the executable:

```
$ odin build src -out:gilded_rose<include your OS executable extension here i.e. .bin>
```

Execute it on the command line with an argument for the number of days:

### macOS:

```
$ ./gilded_rose.bin 10
```
### Windows:

```
$ ./gilded_rose.exe 10
```

## Run the TextTest approval test that comes with this project
There are instructions in the TextTest Readme for setting up TextTest. You will need to specify the executable in `config.gr`. Uncomment this line to use it:

```
#executable:${TEXTTEST_HOME}/odin/gilded_rose<include your OS executable extension here>
```
