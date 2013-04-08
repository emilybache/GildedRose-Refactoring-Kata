require_relative 'yahtzee'
require 'test/unit'

class YahtzeeTest < Test::Unit::TestCase
  def test_chance_scores_sum_of_all_dice
    expected = 15
    actual = Yahtzee.chance(2,3,4,5,1)
    assert expected == actual
    assert 16 == Yahtzee.chance(3,3,4,5,1)
  end

  def test_yahtzee_scores_50
    expected = 50
    actual = Yahtzee.yahtzee([4,4,4,4,4])
    assert expected == actual
    assert 50 == Yahtzee.yahtzee([6,6,6,6,6])
    assert 0 == Yahtzee.yahtzee([6,6,6,6,3])
  end

  def test_1s
    assert Yahtzee.ones(1,2,3,4,5) == 1
    assert 2 == Yahtzee.ones(1,2,1,4,5)
    assert 0 == Yahtzee.ones(6,2,2,4,5)
    assert 4 == Yahtzee.ones(1,2,1,1,1)
  end

  def test_2s
    assert Yahtzee.twos(1,2,3,2,6) == 4
    assert Yahtzee.twos(2,2,2,2,2) == 10
  end

  def test_threes
    assert 6 == Yahtzee.threes(1,2,3,2,3)
    assert 12 == Yahtzee.threes(2,3,3,3,3)
  end

  def test_fours_test
    assert 12 == Yahtzee.new(4,4,4,5,5).fours
    assert 8 == Yahtzee.new(4,4,5,5,5).fours
    assert 4 == Yahtzee.new(4,5,5,5,5).fours
  end

  def test_fives()
    assert 10 == Yahtzee.new(4,4,4,5,5).fives()
    assert 15 == Yahtzee.new(4,4,5,5,5).fives()
    assert 20 == Yahtzee.new(4,5,5,5,5).fives()
  end

  def test_sixes_test
    assert 0 == Yahtzee.new(4,4,4,5,5).sixes()
    assert 6 == Yahtzee.new(4,4,6,5,5).sixes()
    assert 18 == Yahtzee.new(6,5,6,6,5).sixes()
  end

  def test_one_pair
    assert 6 == Yahtzee.score_pair(3,4,3,5,6)
    assert 10 == Yahtzee.score_pair(5,3,3,3,5)
    assert 12 == Yahtzee.score_pair(5,3,6,6,5)
  end

  def test_two_Pair
    assert_equal 16, Yahtzee.two_pair(3,3,5,4,5)
    assert_equal 0, Yahtzee.two_pair(3,3,5,5,5)
  end

  def test_three_of_a_kind()
    assert 9 == Yahtzee.three_of_a_kind(3,3,3,4,5)
    assert 15 == Yahtzee.three_of_a_kind(5,3,5,4,5)
    assert 0 == Yahtzee.three_of_a_kind(3,3,3,3,5)
  end

  def test_four_of_a_knd
    assert 12 == Yahtzee.four_of_a_kind(3,3,3,3,5)
    assert 20 == Yahtzee.four_of_a_kind(5,5,5,4,5)
    assert 0 == Yahtzee.three_of_a_kind(3,3,3,3,3)
  end

  def test_smallStraight()
    assert 15 == Yahtzee.smallStraight(1,2,3,4,5)
    assert 15 == Yahtzee.smallStraight(2,3,4,5,1)
    assert 0 == Yahtzee.smallStraight(1,2,2,4,5)
  end

  def test_largeStraight
    assert 20 == Yahtzee.largeStraight(6,2,3,4,5)
    assert 20 == Yahtzee.largeStraight(2,3,4,5,6)
    assert 0 == Yahtzee.largeStraight(1,2,2,4,5)
  end

  def test_fullHouse()
    assert 18 == Yahtzee.fullHouse(6,2,2,2,6)
    assert 0 == Yahtzee.fullHouse(2,3,4,5,6)
  end
end
