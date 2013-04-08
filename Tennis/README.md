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

# Get going quickly with Cyber-dojo

As an alternative to downloading the code, click one of the links below to create a new cyber-dojo to work in, then press "start" to get going coding.

- [Python](http://cyber-dojo.com/forker/fork/FFEB8EE18C?avatar=cheetah&tag=4)
- [Ruby](http://cyber-dojo.com/forker/fork/9197D6B12C?avatar=cheetah&tag=4)
- [Java](http://cyber-dojo.com/forker/fork/426FA07B60?avatar=raccoon&tag=3)
- [C++](http://cyber-dojo.com/forker/fork/CD6FC41518?avatar=deer&tag=45)
- [C#](http://cyber-dojo.com/forker/fork/672E047F5D?avatar=buffalo&tag=8)