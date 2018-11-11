using System;
using NUnit.Framework;
using TestNinja.Fundamentals;

namespace TestNinja.UnitTests.UnitTests
{
    [TestFixture]
    class DemeritPointsCalculatorTests
    {
        private DemeritPointsCalculator _demeritPointsCalculator;

        [SetUp]
        public void SetUp()
        {
            _demeritPointsCalculator = new DemeritPointsCalculator();
        }

        [Test]
        public void CalculateDemeritPoints_SpeedIsNegative_ThrowArgumentOutOfRangeException()
        {
            Assert.That(() => _demeritPointsCalculator.CalculateDemeritPoints(-9),
                Throws.Exception.TypeOf<ArgumentOutOfRangeException>());
        }

        [Test]
        public void CalculateDemeritPoints_SpeedIsGreaterThenMaxSpeed_ThrowArgumentOutOfRangeException()
        {
            Assert.That(() => _demeritPointsCalculator.CalculateDemeritPoints(350),
                Throws.Exception.TypeOf<ArgumentOutOfRangeException>());
        }

        [Test]
        [TestCase(2)]
        [TestCase(30)]
        [TestCase(64)]
        [TestCase(65)]
        public void CalculateDemeritPoints_SpeedIsLessOrEqualSpeedLimit_ReturnZero(int speed)
        {
            var result = _demeritPointsCalculator.CalculateDemeritPoints(speed);
            Assert.That(result, Is.EqualTo(0));
        }

        [Test]
        [TestCase(66, 0)]
        [TestCase(70, 1)]
        [TestCase(80, 3)]
        public void CalculateDemeritPoints_SpeedIsOverSpeedLimit_ReturnDemeritPoints(int speed, int expectedResult)
        {
            var result = _demeritPointsCalculator.CalculateDemeritPoints(speed);
            Assert.That(result, Is.EqualTo(expectedResult));
        }


    }
}
