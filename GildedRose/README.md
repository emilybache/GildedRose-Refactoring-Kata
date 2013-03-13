This Kata was originally created by Terry Hughes (http://twitter.com/#!/TerryHughes). It is already on GitHub [here](https://github.com/NotMyself/GildedRose). I could have forked it again, but I thought other language users might not want to download a whole C# project environment. In this repository are starting code samples for Java, Python, Ruby, Smalltalk, C#, C and C++.

See also http://iamnotmyself.com/2011/02/13/refactor-this-the-gilded-rose-kata/

## How to use this Kata

The simplest way is to just clone the code and start hacking away improving the design. You'll want to look at the ["Gilded Rose Requirements"](https://github.com/emilybache/Refactoring-Katas/blob/master/GildedRose/GildedRoseRequirements.txt) which explains what the code is for. I strongly advise you that you'll also need some tests if you want to make sure you don't break the code while you refactor.

You could write some unit tests yourself, using the requirements to identify suitable test cases. I've provided a failing unit test in a popular test framework as a starting point for most languages.

Alternatively, use the "Text-Based" tests provided in this repository. (Read more about that in the next section)

I've also set this kata up on [cyber-dojo](http://cyber-dojo.com) for several languages, so you can get going really quickly:

- [Cucumber, Java](http://cyber-dojo.com/forker/fork/0F82D4BA89?avatar=gorilla&tag=45) - for this one I've also written some step definitions for you
- [JUnit, Java](http://cyber-dojo.com/forker/fork/751DD02C4C?avatar=snake&tag=4)
- [C#](http://cyber-dojo.com/forker/fork/107907AD1E?avatar=alligator&tag=13)
- [Ruby](http://cyber-dojo.com/forker/fork/A8943EAF92?avatar=hippo&tag=9)
- [RSpec, Ruby](http://cyber-dojo.com/forker/fork/8E58B0AD16?avatar=raccoon&tag=3)
- [Python](http://cyber-dojo.com/forker/fork/297041AA7A?avatar=lion&tag=4)



Whichever testing approach you choose, the idea of the exercise is to do some deliberate practice, and improve your Refactoring skills. The idea is not to re-write the code from scratch, but rather to practice taking small steps, running the tests often, and incrementally improving the design. 

## Text-Based Testing

This is a testing approach which is very useful when refactoring legacy code. The basic idea is to create tests that use the text which the code produces. Before you change the code, you run it, and save the output as a "Golden Copy". Then after you change the code, you run it again, and compare the output against the Golden Copy. Any differences, and the test fails.

It's basically the same idea as "assertEquals(expected, actual)" in a unit test, except the text you are comparing is typically much longer, and the "expected" value is saved from actual output, rather than being defined in advance.

Typically a piece of legacy code may not produce suitable textual output from the start, so you may need to modify it before you can write your first text-based test. That could involve inserting log statements into the code, or just writing a "main" method that executes the code and prints out what the result is afterwards. It's this latter approach we are using here to test GildedRose.

The Text-Based tests in this repository are designed to be used with the tool "TextTest" (http://texttest.org). This tool helps you to organize and run text-based tests. There is more information in the README file in the "texttests" subdirectory.

