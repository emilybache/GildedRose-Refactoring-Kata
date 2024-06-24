Contributing to Gilded Rose Refactoring Kata
======

More translations are most welcome! I'm very open for pull requests that
translate the starting position into additional languages. However, please
do **not** open a pull request with your solution! It can be a bit confusing since
GitHub encourages you to do so! Please only send me pull requests if you have a
correction or improvement to the starting position. You don't want to spoil the
fun of doing the exercise for other people!

# Translating this code

Please note a translation should ideally include:

- a translation of the production code for 'update_quality' and Item
- one failing unit test complaining that "fixme" != "foo"
- a TextTest fixture, i.e. a command-line program that runs update_quality on the sample data for the number of days specified

Please don't write too much code in the starting position or add too many unit
tests. The idea with the one failing unit test is to tempt people to work out
how to fix it, discover it wasn't that hard, and now they understand what this
test is doing they realize they can improve it.

If your programming language doesn't have an easy way to add a command-line
interface, then the TextTest fixture is probably not necessary.

# Recommended project structure

Programming languages have a variety of conventions but the starting points try
to maintain order among languages. Ideally, the 'update_quality' and
Item definitions should be in a file named `gilded_rose` with your language's
conventional casing (e.g. snake case) and location (e.g. `src/`). The "fixme" !
= "foo" test should go in a file `gilded_rose_test` in your language's
conventional location (e.g. `test/`). The TextTest fixture and command-line
program, that simulates update_quality over a number of days, should go in
`program` or `texttest_fixture`. If you can define a default for the number of
days in the simulation please choose two days.

A single sub-directory per language is not enforced. A language may have
more than one popular unit testing framework. In that case, please add
`{language}-{framework}/` and maintain separation between the projects. In other
words, all the components requested should exist in both sub-directories.
Re-using code between the directories would be confusing for those looking for a
starting point.
