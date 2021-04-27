# Gilded Rose Refactoring Kata

Introduction:
I will try as much as I can to write my thought processes here as it's one of the requirements. 
I will just write the relevant parts and not my whole thoughts. That would be weird since I was thinking of lunch just before I started. :D

SIDE COMMENTS:
- I couldn't exactly do a full TDD since the application is already written. TDD requires tests before development but this is a refactoring.
- One of the things I would ideally change if it was not explicitly said to not to is the fact that it uses strings to check. that can be really erroneous. 
  If I had my way, I would include IDs of some sort.

1:
One of the first things I want to do is just get the program to run. so am not fixing anything except what will make the program run.
- The program seems to run fine. except I could not see the results so added a little line to see what was printed. over than that. it looks good. 
  Makes sense why they kept using it. It works! :/

2:
After this I want to include some Unit tests to ensure all parts are working as they should. This may require some fixing. Not sure.
- First thing I did was separate the tests from the main application. I do not want to have the possibility of a memory leak or bulky apps
- Original unit tests were written in NUnit and I chose to use MSTest. Why? no reason! Just a choice.

3:
Then when all tests are good. I can start working with an aim of not breaking any tests.