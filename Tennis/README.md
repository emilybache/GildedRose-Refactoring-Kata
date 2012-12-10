# Tennis Refactoring Kata

There are three versions of this refactoring kata, each with their own design smells and challenges. Two have been written by junior programmers with a weak grasp of how to solve the problem. The third is by an evil genius who values compactness above all else. The test suite provided is fairly comprehensive, and fast to run. You should not need to change the tests, only run them often as you refactor.

## Tennis Kata

Tennis has a rather quirky scoring system, and to newcomers it can be a little difficult to keep track of. The tennis society has contracted you to build a scoreboard to display the current score during tennis games. 

Your task is to write a “TennisGame” class containing the logic which outputs the correct score as a string for display on the scoreboard. When a player scores a point, it triggers a method to be called on your class letting you know who scored the point. Later, you will get a call “score()” from the scoreboard asking what it should display. This method should return a string with the current score.

You can read more about Tennis scores [here](http://en.wikipedia.org/wiki/Tennis#Scoring) which is summarized below:

1. A game is won by the first player to have won at least four points in total and at least two points more than the opponent.
2. The running score of each game is described in a manner peculiar to tennis: scores from zero to three points are described as "love", "fifteen", "thirty", and "forty" respectively.
3. If at least three points have been scored by each player, and the scores are equal, the score is "deuce".
4. If at least three points have been scored by each side and a player has one more point than his opponent, the score of the game is "advantage" for the player in the lead.

You need only report the score for the current game. Sets and Matches are out of scope.

## Get going quickly with Cyber-dojo

I have set up some cyber-dojos with the starting code. This should make it very quick to get started with the kata. The other advantage of cyber-dojo is that if you make a refactoring mistake, it's very easy to revert the code to the last green bar, and practice the refactoring again. Just click on the most recent green traffic light shown in the list above the code.

- [Python](http://cyber-dojo.com/forker/fork/FFEB8EE18C?avatar=cheetah&tag=3)
- [Ruby](http://cyber-dojo.com/forker/fork/9197D6B12C?avatar=cheetah&tag=3)
- [Java](http://cyber-dojo.com/forker/fork/B22DCD17C3?avatar=buffalo&tag=11)
- I havn't done C++ yet, since cyber-dojo is using a different test framework.