using NUnit.Framework;

[TestFixture]
public class UntitledTest
{
    [Test]
    public void Chance_scores_sum_of_all_dice()
    {
        int expected = 15;
        int actual = Yahtzee.Chance(2,3,4,5,1);
        Assert.AreEqual(expected, actual);
        Assert.AreEqual(16, Yahtzee.Chance(3,3,4,5,1));
    }

    [Test]
    public void Yahtzee_scores_50() 
    {
        int expected = 50;
        int actual = Yahtzee.yahtzee(4,4,4,4,4);
        Assert.AreEqual(expected, actual);
        Assert.AreEqual(50, Yahtzee.yahtzee(6,6,6,6,6));
        Assert.AreEqual(0, Yahtzee.yahtzee(6,6,6,6,3));
    }

    [Test]
    public void Test_1s() {
        Assert.IsTrue(Yahtzee.Ones(1,2,3,4,5) == 1);
        Assert.AreEqual(2, Yahtzee.Ones(1,2,1,4,5));
        Assert.AreEqual(0, Yahtzee.Ones(6,2,2,4,5));
        Assert.AreEqual(4, Yahtzee.Ones(1,2,1,1,1));
    }

    [Test]
    public void test_2s() 
    {
        Assert.AreEqual(4, Yahtzee.Twos(1,2,3,2,6));
        Assert.AreEqual(10, Yahtzee.Twos(2,2,2,2,2));
    }

    [Test]
    public void test_threes() 
    {
        Assert.AreEqual(6, Yahtzee.Threes(1,2,3,2,3));
        Assert.AreEqual(12, Yahtzee.Threes(2,3,3,3,3));
    }

    [Test]
    public void fours_test() 
    {
        Assert.AreEqual(12, new Yahtzee(4,4,4,5,5).Fours());
        Assert.AreEqual(8, new Yahtzee(4,4,5,5,5).Fours());
        Assert.AreEqual(4, new Yahtzee(4,5,5,5,5).Fours());
    }

    [Test]
    public void fives() {
        Assert.AreEqual(10, new Yahtzee(4,4,4,5,5).Fives());
        Assert.AreEqual(15, new Yahtzee(4,4,5,5,5).Fives());
        Assert.AreEqual(20, new Yahtzee(4,5,5,5,5).Fives());
    }

    [Test]
    public void sixes_test() 
    {
        Assert.AreEqual(0, new Yahtzee(4,4,4,5,5).sixes());
        Assert.AreEqual(6, new Yahtzee(4,4,6,5,5).sixes());
        Assert.AreEqual(18, new Yahtzee(6,5,6,6,5).sixes());
    }

    [Test]
    public void one_pair() 
    {
        Assert.AreEqual(6, Yahtzee.ScorePair(3,4,3,5,6));
        Assert.AreEqual(10, Yahtzee.ScorePair(5,3,3,3,5));
        Assert.AreEqual(12, Yahtzee.ScorePair(5,3,6,6,5));
    }

    [Test]
    public void two_Pair() 
    {
        Assert.AreEqual(16, Yahtzee.TwoPair(3,3,5,4,5));
        Assert.AreEqual(0, Yahtzee.TwoPair(3,3,5,5,5));
    }

    [Test]
    public void three_of_a_kind() 
    {
        Assert.AreEqual(9, Yahtzee.ThreeOfAKind(3,3,3,4,5));
        Assert.AreEqual(15, Yahtzee.ThreeOfAKind(5,3,5,4,5));
        Assert.AreEqual(0, Yahtzee.ThreeOfAKind(3,3,3,3,5));
    }

    [Test]
    public void four_of_a_knd() 
    {
        Assert.AreEqual(12, Yahtzee.FourOfAKind(3,3,3,3,5));
        Assert.AreEqual(20, Yahtzee.FourOfAKind(5,5,5,4,5));
        Assert.AreEqual(0, Yahtzee.FourOfAKind(3,3,3,3,3));
    }

    [Test]
    public void smallStraight() 
    {
        Assert.AreEqual(15, Yahtzee.SmallStraight(1,2,3,4,5));
        Assert.AreEqual(15, Yahtzee.SmallStraight(2,3,4,5,1));
        Assert.AreEqual(0, Yahtzee.SmallStraight(1,2,2,4,5));
    }

    [Test]
    public void largeStraight() 
    {
        Assert.AreEqual(20, Yahtzee.LargeStraight(6,2,3,4,5));
        Assert.AreEqual(20, Yahtzee.LargeStraight(2,3,4,5,6));
        Assert.AreEqual(0, Yahtzee.LargeStraight(1,2,2,4,5));
    }

    [Test]
    public void fullHouse() 
    {
        Assert.AreEqual(18, Yahtzee.FullHouse(6,2,2,2,6));
        Assert.AreEqual(0, Yahtzee.FullHouse(2,3,4,5,6));
    }
}

